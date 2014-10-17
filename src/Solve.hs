module Solve where
import Prelude hiding (foldl, sum, concatMap)
import qualified Data.Heap as H
import qualified Data.Map as Map
import Data.Foldable
import Linear
import Control.Lens
import System.Random
import Control.Applicative

data Problem = Problem
  { choiceCost :: Double
  , swapCost :: Double
  , rounds :: Int
  , columns :: Int
  , rows :: Int
  , evaluate :: Field -> Double
  , isComplete :: Field -> Bool }

type Field = Map.Map (V2 Int) (V2 Int)

data Direction = U | D | L | R deriving (Show, Eq)

dir2V2 :: Direction -> V2 Int
dir2V2 U = V2 0 (-1)
dir2V2 D = V2 0 1
dir2V2 L = V2 (-1) 0
dir2V2 R = V2 1 0

data Op = Op (V2 Int) [Direction] deriving (Show, Eq)

type C = H.Entry Double ([Op], Field)

screen :: H.Heap C -> [C]
screen h = toList h

generate :: Problem -> C -> IO (H.Heap C)
generate prob e = do
  return $ H.fromList $ concatMap (flip (swap prob) e) [U,D,L,R]
    ++ concatMap (flip (choice prob) e) (V2 <$> [0..columns prob-1] <*> [0..rows prob-1])

swap :: Problem -> Direction -> C -> [C]
swap prob dir (H.Entry _ (Op t ds : ops, f)) = do
  let t' = t + dir2V2 dir
  p <- f ^.. ix t
  q <- f ^.. ix t'
  let f' = f & ix t .~ q & ix t' .~ p
  return $ evalE prob (Op t' (dir : ds) : ops) f'
swap _ _ _ = []

evalE :: Problem -> [Op] -> Field -> C
evalE prob ops f = H.Entry (evaluate prob f
  + choiceCost prob * fromIntegral (length ops)
  + sum (map (evalOp prob) ops)) (ops, f)

evalOp :: Problem -> Op -> Double
evalOp prob (Op _ ops) = swapCost prob * fromIntegral (length ops)

choice :: Problem -> V2 Int -> C -> [C]
choice prob v (H.Entry _ (ops, f)) = [evalE prob (Op v [] : ops) f | length ops < rounds prob]

solve prob f0 = go $ H.singleton (evalE prob [] f0) where
  go hs = do
    let hs0 = screen hs
    case filter (isComplete prob . snd . H.payload) hs0 of
      [] -> do
        hs' <- mapM (generate prob) hs0
        go (foldl H.union H.empty hs')
      (H.Entry _ (r, _):_) -> return r