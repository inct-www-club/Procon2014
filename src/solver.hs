data Way = U | R | D | L deriving(Eq)
-- TODO : ~Opをモジュール化する
data Op = Op { opPiece :: Int
             , opCost :: Int
             , opWay :: [Way] }
type Field = [Int]

swap_cost = 1
choice_cost = 10
fieldWidht = 20
fieldHeight = 20

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

swap :: Int -> Way -> Field -> Maybe Field
swap piece way field = case (afterIndex piece way) of
  (Just piece') -> Just $ swapByIndex piece piece' field
  (Nothing) -> Nothing

afterIndex :: Int -> Way -> Maybe Int
afterIndex index way
  | way == U = if afterU >= 0
                  then Just afterU
                  else Nothing
  | way == R = if (mod index fieldWidth) + 1 < fieldWidth
                  then Just $ index + 1
                  else Nothing
  | way == D = if afterD < fieldLength
                  then Just afterD
                  else Nothing
  | way == L = if (mod index fieldWidth) - 1 >= 0
                  then Just $ index - 1
                  else Nothing
  where fieldLength = fieldWidth * fieldHeight
        afterU = index - fieldWidth
        afterD = index + fieldWidth

swapByIndex :: Int -> Int -> Field -> Field
swapByIndex i j xs = reverse $ fst $ foldl f ([],0) xs
    where
      f (acc, idx) x
        | idx == i  = (xs!!j:acc, idx+1)
        | idx == j  = (xs!!i:acc, idx+1)
        | otherwise = (x:acc, idx+1)
