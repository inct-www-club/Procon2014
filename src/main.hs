import Solve as S
import qualified Data.Map as Map
import Control.Applicative
import Linear
import Control.Monad
simpleProblem :: S.Problem
simpleProblem = S.Problem
  { choiceCost = 3
  , swapCost = 1
  , rounds = 3
  , columns = 3
  , rows = 3
  , evaluate = fromIntegral . length . filter id . f
  , isComplete = and . f }
  where
    f = zipWith (==) simpleIndices . Map.elems

simpleIndices = V2 <$> [0..2] <*> [0..2]

runSolver :: [Int] -> IO ()
runSolver ps = do
  let f0 = zip simpleIndices [V2 d m | p <- ps, let (d, m) = divMod p 3]
  print f0
  r <- solve simpleProblem $ Map.fromList f0
  print r

main = forever $ readLn >>= runSolver