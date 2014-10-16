data Way = U | R | D | L deriving(Eq, Show)
data Op = Op { opPiece :: Int
             , opCost :: Int
             , opWay :: [Way] } deriving(Eq, Show)
type Field = [Int]

swapCost = 1
choiceCost = 10
fieldWidth = 5
fieldHeight = 5

solver :: Field -> Field -> Int -> Way -> Maybe [Op]
solver goalField field piece way
  | goalField == field = Just [(Op (-1) 0 [])] -- 終端
  | piece' == Nothing = Nothing
  | best == Nothing = Nothing
  | opPiece(op) == pieceVal = Just ( (Op piece (opCost(op) + swapCost) (way:opWay(op))) : opx )
  | otherwise = Just ( (Op piece choiceCost [way]) : (op : opx))
  where
    (Just (op:opx)) = best
    best = choice goalField field'
    field' = swapByIndex piece pieceVal field
    piece' = afterIndex piece way
    (Just pieceVal) = piece'

choice :: Field -> Field -> Maybe [Op]
choice goalField field = bestOp $ zipWith (solver goalField field) [0..(length(field) - 1)] [U, R, D, L]

bestOp :: [Maybe [Op]] -> Maybe [Op]
bestOp [] = error "empty list"
bestOp [x] = x
bestOp (x:xs) = rase minOp x (bestOp xs)

minOp :: [Op] -> [Op] -> [Op]
minOp xs ys
  | xsCost <= ysCost = xs
  | otherwise        = ys
  where xsCost = sum $ map opCost xs
        ysCost = sum $ map opCost ys

rase :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
rase f (Just a) (Just b) = Just (f a b)
rase _ (Just a) Nothing  = Just a
rase _ Nothing (Just b)  = Just b
rase _ Nothing Nothing   = Nothing


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
