module Solve where
import Prelude hiding (foldl, sum, concatMap)
import qualified Data.Heap as H
import qualified Data.Map.Strict as Map
import Data.Foldable
import Linear
import Control.Lens
import System.Random
import Control.Applicative
import System.IO
import Data.Array

data Problem = Problem
  { choiceCost :: Double
  , swapCost :: Double
  , rounds :: Int
  , columns :: Int
  , rows :: Int
  , evaluate :: Field -> Double
  , isComplete :: Field -> Bool }

type Field = Array (V2 Int) (V2 Int)

dist :: V2 Int -> V2 Int -> Int
dist (V2 a b) (V2 c d) = abs (a - c) + abs (b - d)

data Direction = U | D | L | R deriving (Show, Eq)

opposite :: Direction -> Direction
opposite U = D
opposite D = U
opposite L = R
opposite R = L

dir2V2 :: Direction -> V2 Int
dir2V2 U = V2 0 (-1)
dir2V2 D = V2 0 1
dir2V2 L = V2 (-1) 0
dir2V2 R = V2 1 0

data Op = Op !Double (V2 Int) (V2 Int) [Direction] deriving (Show, Eq)

type C = H.Entry Double ([Op], Field)

generate :: Problem -> C -> IO (H.Heap C)
generate prob e = do
  return $ H.fromList $ concatMap (flip (swap prob) e) [U,D,L,R]
    ++ concatMap (flip (choice prob) e) (V2 <$> [0..columns prob-1] <*> [0..rows prob-1])

swap :: Problem -> Direction -> C -> [C]
swap prob dir (H.Entry _ (Op cost t0 t ds : ops, f))
  | (d0 : _) <- ds, opposite d0 == dir = []
  | otherwise = do
    let t' = t + dir2V2 dir
    p <- f ^.. ix t
    q <- f ^.. ix t'
    let f' = f & ix t .~ q & ix t' .~ p
    return $ evalE prob (Op (cost + swapCost prob) t0 t' (dir : ds) : ops) f'
swap _ _ _ = []

evalE :: Problem -> [Op] -> Field -> C
evalE prob ops f = H.Entry (evaluate prob f + sum (map opCost ops)) (ops, f)

opCost :: Op -> Double
opCost (Op c _ _ _) = c

choice :: Problem -> V2 Int -> C -> [C]
choice prob v (H.Entry _ (ops, f)) = [evalE prob (Op (choiceCost prob) v v [] : ops) f | length ops < rounds prob]

solve prob f0 = go 0 $ H.singleton (evalE prob [] f0) where
  go n hs0 = do
    let hs
          | n `mod` 16 == 0 = take 1000 $ toList hs0
          | otherwise = take 500 $ toList hs0
    case hs of
      (H.Entry p _:_) -> do
        putStrLn $ show p
        hFlush stdout
      [] -> fail "Unsolvable"
    case filter (isComplete prob . snd . H.payload) hs of
      [] -> do
        hs' <- mapM (generate prob) hs
        go (n + 1) (foldl H.union H.empty hs')
      (H.Entry _ (r, _):_) -> return r