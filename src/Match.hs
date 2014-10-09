import Data.Complex
import Data.Direction
import Control.Applicative
import qualified Data.Set as Set

type RealPiece

fromImage :: C.Image PixelRGBA8 -> [RealPiece]
fromImage = undefined

type Chara = [Complex]

distanceC :: Chara -> Chara -> Double
distanceC a b = sum $ map (^2) $ zipWith subtract a b

gen :: Freq -> Phase -> [Double] -> Complex Double
gen _ _ [] = 0
gen f p (x:xs) = cis p * x + gen f (p + f) xs 

data Piece a = Piece
    Int
    Chara -- top
    Chara -- bottom
    Double -- Local divergence
    a

instance Monoid a => Monoid (Piece a) where
    mempty = Piece 0 [] [] 0 mempty
    mappend (Piece m p q x a) (Piece n r s y b) = Piece (m + n) p s (distanceC q r) (mappend a b)

data Space a = Tip a | Space (V4 (V4 (Space a)))

release :: [Piece a] -> Space (Piece a)

popNearest :: Chara -> Space (Piece a) -> Maybe (Piece a, Space (Piece a)) -- O(logN) if possible

merge :: Eq a => Space (Piece a) -> [Piece a] -> [Piece a]
merge sp (x:xs) = undefined
merge sp [] = []

-- 0.
-- 1. calculate the characteristics for every edge
-- 2. map them into a N-dimentional space (N=4?)
-- 3. then just merge...