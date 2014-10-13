import Data.Complex
import Control.Applicative
import qualified Data.Foldable as F
import qualified Data.Set as Set
import Linear
import Codec.Picture
import System.Random

type RealPiece = Piece [Piece (V2 Int)]

fromImage :: Image PixelRGB8 -> Int -> Int -> [RealPiece]
fromImage img c r = [Piece 1
  (squash [pixelAt img x (h * r) | x <- [w*c..w*succ c-1]])
  (squash [pixelAt img x (h * succ r - 1) | x <- [w*c..w*succ c-1]])
  0
  [Piece
    1
    (squash [pixelAt img (w * c) y | y <- [h*r..h*succ r-1]])
    (squash [pixelAt img (w * succ c - 1) y | y <- [h*r..h*succ r-1]])
    0
    (V2 c r)]
  | c <- [0..c-1], r <- [0..r-1]] where
  w = imageWidth img `div` c
  h = imageHeight img `div` r

squash :: [PixelRGB8] -> Chara
squash ps = V4 (gen 0.1 0 xs) (gen 0.1 0 ys) (gen 0.1 0 zs) (gen 0.2 0 $ zipWith (+) xs ys) where
  (xs, ys, zs) = foldr 
    (\(PixelRGB8 r g b) (rs, gs, bs) -> (fromIntegral r : rs, fromIntegral g : gs, fromIntegral b : bs))
    ([], [], [])
    ps

shrink :: IM.IntMap (Piece a) -> IM.IntMap (Piece a)
shrink n m = id

combine (Piece m p q x a) (Piece n r s y b) = Piece (m + n) p s (distanceC q r) (a ++ b)

type Chara = V4 (Complex Double)

distanceC :: Chara -> Chara -> Double
distanceC a b = F.sum $ fmap g2 $ liftA2 subtract a b where
  g2 :: Complex Double -> Double
  g2 (a :+ b) = a^2 + b^2

gen :: Double -> Double -> [Double] -> Complex Double
gen _ _ [] = 0
gen f p (x:xs) = cis p * (x :+ 0) + gen f (p + f) xs 

data Piece a = Piece
    Int
    Chara -- top
    Chara -- bottom
    { divergence :: Double } -- Local divergence
    a

-- 0.
-- 1. calculate the characteristics for every edge
-- 2. map them into a N-dimentional space (N=4?)
-- 3. then just merge...