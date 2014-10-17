{-# LANGUAGE ViewPatterns, MultiWayIf #-}
module Match where
import Codec.Picture
import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Array
import Data.ByteString.Lens
import Data.Complex
import Data.Function (on)
import Data.List (sort, sortBy, transpose)
import Data.String
import Debug.Trace
import Foreign.Ptr
import Foreign.Storable
import Graphics.Netpbm
import Linear
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MVS
import System.Random
import Unsafe.Coerce

insert :: (a -> Complex Double) -> a -> Space a -> Space a
insert f p (Space siz ar) = Space siz $ accum (flip (:)) ar [(toSpaceIndex (f p) siz, (f p, p))]

insertMany :: (Piece a -> Complex Double) -> [Piece a] -> Space (Piece a) -> Space (Piece a)
insertMany f = flip $ foldr (insert f)

instance Eq a => Eq (Entry a b) where
  Entry a _ == Entry b _ = a == b
  
instance Ord a => Ord (Entry a b) where
  compare (Entry a _) (Entry b _) = compare a b

nearest :: Eq b => (a -> b) -> a -> [Entry Double a] -> [Entry Double a]
nearest f p (e@(Entry v p') : s')
  | f p == f p' = nearest f p s'
  | otherwise = e : nearest f p s'
nearest f p [] = []

data Adj = Vertical | Horizontal
data Entry a b = Entry { entryWeight :: a, entryData :: b }

data Piece a = Piece
  { pieceTop :: V.Vector (Complex Double)
  , pieceBottom :: V.Vector (Complex Double)
  , pieceLeft :: V.Vector (Complex Double)
  , pieceRight :: V.Vector (Complex Double)
  , pieceExtract :: a
  }
type RealPiece = Piece (V2 Int)

data ProblemInfo = ProblemInfo
  { selectCost :: Int
  , swapCost :: Int
  , rounds :: Int
  , columns :: Int
  , rows :: Int
  , theImage :: Image PixelRGB8
  }
data Space a = Space (V2 Int) (Array (V2 Int) [(Complex Double, a)])
 
-- combine (Piece m p q x a) (Piece n r s y b) = Piece (m + n) p s (distanceC q r) (a ++ b)
averageM :: V.Vector (Complex Double) -> Double
averageM v = V.foldl (\r x -> r + magnitude x) 0 v / fromIntegral (V.length v)

emptySpace :: Int -> Space a
emptySpace n = Space (V2 n n) (listArray (-V2 n n, V2 n n) $ repeat [])

find :: Complex Double -> Space (Piece a) -> [Entry Double (Piece a)]
find z s@(Space siz ar) = sort $ map (\(d, a) -> Entry (quadranceC $ z - d) a) $ concat
  $ concat [ar ^.. ix i
  , ar ^.. ix (i + V2 0 1)
  , ar ^.. ix (i + V2 1 0)
  , ar ^.. ix (i - V2 0 1)
  , ar ^.. ix (i - V2 1 0)] where
    i = toSpaceIndex z siz

fourier :: Int -> [PixelRGB8] -> V.Vector (Complex Double)
fourier n ps = V.fromList $ concat $ transpose [ff red, ff green, ff blue] where
  ff f = [gen (2 * pi * fromIntegral k / fromIntegral n) 0 (map f ps) / fromIntegral n | k <- takeWhile (<n`div`2) $ iterate (*2) 1]
  blue (PixelRGB8 _ _ b) = fromIntegral b / 256
  green (PixelRGB8 _ g _) = fromIntegral g / 256
  red (PixelRGB8 r _ _) = fromIntegral r / 256
  gen _ _ [] = 0
  gen f p (x:xs) = cis p * (x :+ 0) + gen f (p + f) xs 

fromImage :: Image PixelRGB8 -> Int -> Int -> [RealPiece]
fromImage img c0 r0 = [Piece
  (fourier w [pixelAt img x (h * r) | x <- [w*c..w*succ c-1]])
  (fourier w [pixelAt img x (h * succ r - 1) | x <- [w*c..w*succ c-1]])
  (fourier h [pixelAt img (w * c) y | y <- [h*r..h*succ r-1]])
  (fourier h [pixelAt img (w * succ c - 1) y | y <- [h*r..h*succ r-1]])
  (V2 c r)
  | c <- [0..c0-1], r <- [0..r0-1]] 
  where
    h = imageHeight img `div` r0  
    w = imageWidth img `div` c0

genPairs :: ProblemInfo -> Adj -> (V.Vector (Complex Double) -> Complex Double) -> [Entry Double (RealPiece, RealPiece)]
genPairs prob mode key = sort $ do
  p <- ps
  Entry d p' <- take 2 $ nearest pieceExtract p $ flip find space $ g p
  return $ Entry d (p, p')
  where
    (f, g) = case mode of { Vertical -> (key . pieceTop, key . pieceBottom)
      ; Horizontal -> (key . pieceLeft, key . pieceRight) }
    ps = fromImage (theImage prob) (columns prob) (rows prob)
    space = insertMany f ps (emptySpace 8)

loadProblem :: FilePath -> IO ProblemInfo
loadProblem path = do
  b <- B.readFile path
  let Right ([PPM (PPMHeader P6 w h) (PpmPixelDataRGB8 v)], _) = parsePPM b
      (header, _) = B.breakSubstring (view packedChars $ show w ++ " " ++ show h) b
      [_, [c, r], [n], [rc, rs]] = map (map read.words.tail) $ lines $ packedChars # header
  return $ ProblemInfo rc rs n c r $ Image w h $ VS.fromList $ concatMap convert $ VS.toList v
  where
    convert (PpmPixelRGB8 r g b) = [r, g, b]
    
quadranceC :: Complex Double -> Double
quadranceC (a :+ b) = a^2 + b^2

realIndex :: RealPiece -> V2 Int
realIndex = pieceExtract

toSpaceIndex :: Complex Double -> V2 Int -> V2 Int
toSpaceIndex (x :+ y) siz = fmap truncate $ V2 x y * fmap fromIntegral siz where

generate :: ProblemInfo -> [Map.Map (V2 Int) (Entry Double (V2 Int))]
generate prob = do
  h <- entire
  r <- tryFill (Set.delete h $ Set.fromList entire, Map.fromList [(V2 0 0, Entry 0 h)]) (tail entire)
  return r
  where
    hs = genPairs prob Horizontal (sum . mapM (flip V.unsafeIndex) [0,1,2,3,4,5])
    vs = genPairs prob Vertical (sum . mapM (flip V.unsafeIndex) [0,1,2,3,4,5])
    mDown = Map.fromListWith (++) [(realIndex p, [Entry d $ realIndex q]) | Entry d (p, q) <- vs]
    mRight = Map.fromListWith (++) [(realIndex p, [Entry d $ realIndex q]) | Entry d (p, q) <- hs]
    entire = V2 <$> [0..columns prob - 1] <*> [0..rows prob - 1]
    tryFill (_, m) [] = return m
    tryFill (available, m) (i:is) = do
      let rs = [e | Entry _ r <- m ^.. ix (i - V2 1 0), e@(Entry _ k) <- concat (mRight ^.. ix r), Set.member k available]
      let ds = [e | Entry _ r <- m ^.. ix (i - V2 0 1), e@(Entry _ k) <- concat (mDown ^.. ix r), Set.member k available]
      case rs ++ ds of
        [] -> []
        xs -> do
          c@(Entry _ k) <- take 5 $ sort xs
          tryFill (Set.delete k available, Map.insert i c m) is
