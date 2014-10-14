data Way = U | R | D | L
-- TODO : ~Opをモジュール化する
data Op = Op { opPiece :: Int
             , opCost :: Int
             , opWay :: [Way] }
type Field = [Int]

swap_cost = 1
choice_cost = 10

solver :: Field -> Field -> Int -> Way -> [Op]
solver goalField field piece way
  | goalField == field'  = [newOp] -- Goal!
  | opPiece(op) == piece = (Op piece (opCost(op) + swap_cost) way:opWay(op)):ops
  | otherwise            = newOp:ops
  where field' = swap field piece way
        choice' = goalField field'
        piece' = swapPiece piece way
        best = bestOp $ choice': $ map (solver goalField field' piece') [U,R,D,L]
        op:ops = best
        newOp = Op piece (choice_cost + swap_cost) [way]

choice :: Field -> Field -> [Op]
choice goalField field = bestOp $ zipWith (solver goalField field) [0..length(field)-1] [U,R,D,L]

bestOp :: [[Op]] -> [Op]
bestOp [] = error "empty list"
bestOp [x] = x
bestOp (x:xs) = minOp x $ bestOp xs

minOp :: [Op] -> [Op] -> [Op]
minOp [] xs = xs
minOp xs [] = xs
minOp xs ys
  | xsCost <= ysCost = x
  | otherwise        = y
  where totalCost = sum $ map opCost
        xsCost = totalCost xs
        ysCost = totalCost ys
