module House (
  Board, play
) where

type Stone = Int
type Board = ([Stone], [Stone])

play :: Board -> Int -> Either Board String
play board position
  | position < 0 || position >= length (fst board) = Right "Invalid Position"
  | otherwise = Left $ sow (position + 1) (fst board !! position) . replace position 0 $ board

replace :: Int -> Stone -> Board -> Board
replace position value (mine, thiers)
  | odd boardRem = (mine, rebuild value (splitAt (position - boardRem * 7) thiers))
  | otherwise    = (rebuild value (splitAt (position - boardRem * 7) mine), thiers)
  where boardRem = div position 7
  
rebuild :: Int -> ([Stone], [Stone]) -> [Stone]
rebuild value (x,_:xs) = x ++ value : xs
rebuild _ (_, []) = []

sow :: Int -> Stone -> Board -> Board
sow position 1 (mine, thiers)
  | odd boardRem = replace position (thiers !! (position - boardRem * 7) + 1) (mine, thiers)
  | otherwise    = replace position (mine !! (position - boardRem * 7) + 1) (mine, thiers)
  where boardRem = div position 7
sow position value board  = (sow (position + 1) (value - 1) . sow position 1) board

--14 15 16 17 18 19 20
--7  8  9  10 11 12 s13
--0  1  2  3  4  5  s6
