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
replace position value board
  | odd quotient = (fst board, rebuild value (splitAt offset (snd board)))
  | otherwise    = (rebuild value (splitAt offset (fst board)), snd board)
  where quotient = div position 7
        offset = position - quotient * 7

sow :: Int -> Stone -> Board -> Board
sow position 1 board
  | odd quotient = replace position (snd board !! offset + 1) board
  | otherwise = replace position (fst board !! offset + 1) board
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
    . replace position 0
    $ board
