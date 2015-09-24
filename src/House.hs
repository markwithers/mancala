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

replace :: Int -> Stone -> Board -> Board
replace position value (b1, b2)
  | odd quotient = (b1, rebuild value (splitAt offset b2))
  | otherwise    = (rebuild value (splitAt offset b1), b2)
  where quotient = div position 7
        offset = position - quotient * 7

sow :: Int -> Stone -> Board -> Board
sow position 1 (b1, b2)
  | odd quotient = replace position (b2 !! offset + 1) (b1, b2)
  | otherwise = replace position (b1 !! offset + 1) (b1, b2)
  where quotient = div position 7
        offset = position - quotient * 7
sow position value board = sow (position + 1) (value - 1)
  . sow position 1
  $ board

play :: Board -> Int -> Either Board String
play board position
  | position < 0 || position >= length (fst board) = Right "Invalid Position"
  | otherwise = Left
    . sowRight position (fst board)
    . (replace position 0)
    $ board
