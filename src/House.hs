module House (
  Board, play
) where

type Stone = Int
type Board = ([Stone], [Stone])

rebuild :: Int -> ([Stone], [Stone]) -> [Stone]
rebuild value (x,_:xs) = x ++ value : xs
rebuild _ (_, []) = []

sowRight :: Int -> [Stone] -> Board -> Board
sowRight position row = sow (position + 1) (row !! position)

replace :: Int -> Stone -> [Stone] -> [Stone]
replace position value row = rebuild value (splitAt position row)

sow :: Int -> Stone -> Board -> Board
sow position 1 (row1, row2)
  | odd quotient = (row1, replace offset (row2 !! offset + 1) row2)
  | otherwise = (replace offset (row1 !! offset + 1) row1, row2)
  where quotient = div position 7
        offset = position - quotient * 7
sow position value board = sow (position + 1) (value - 1)
  . sow position 1
  $ board

play :: Board -> Int -> Either Board String
play (row1, row2) position
  | position < 0 || position >= length row1 = Right "Invalid Position"
  | otherwise = Left
    . sowRight position row1
    $ (replace position 0 row1, row2)
