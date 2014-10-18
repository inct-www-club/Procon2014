{-# LANGUAGE LambdaCase, ViewPatterns #-}
import Solve as S
import qualified Match as M
import qualified Data.Map.Strict as Map
import Control.Applicative
import Linear
import Control.Monad
import Data.Array
import System.Environment

simpleProblem :: Int -> Int -> S.Problem
simpleProblem c r = S.Problem
  { choiceCost = 5
  , swapCost = 2
  , rounds = 4
  , columns = c
  , rows = r
  , evaluate = \field -> fromIntegral $ sum $ map (uncurry S.dist) (fAssocs field)
  , isComplete = \field -> (0==) $ fromIntegral $ sum $ map (uncurry S.dist) (fAssocs field) }

simpleIndices c r = flip V2 <$> [0..r - 1] <*> [0..c - 1]

runSolver :: Int -> Int -> [Int] -> IO ()
runSolver c r ps = do
  let f0 = zip (simpleIndices c r) [V2 d m | p <- ps, let (m, d) = divMod p c]
  print f0
  r <- solve (simpleProblem c r) $ fBuild c r f0
  putStrLn ""
  forM_ (reverse r) $ \(Op cost t0 _ os) -> print (t0, reverse os)

main = getArgs >>= \case
  ((read -> c) : (read -> r) : _) -> readLn >>= runSolver c r
