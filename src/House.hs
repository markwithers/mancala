module House (
  Board, play
) where

type Stone = Int
type Board = ([Stone], [Stone])

place :: Int -> ([Stone], [Stone]) -> [Stone]
place stones (x,_:xs) = x ++ stones : xs
place _ (_, []) = []

sowRight :: Int -> [Stone] -> Board -> Board
sowRight position board = sow (position + 1) (board !! position)

replace :: Int -> Stone -> [Stone] -> [Stone]
replace position stones board = place stones (splitAt position board)

sow :: Int -> Stone -> Board -> Board
sow position 1 (playerBoard, opponentBoard)
  | odd quotient = (playerBoard, inc opponentBoard)
  | otherwise = (inc playerBoard, opponentBoard)
  where quotient = div position 7
        offset = position - quotient * 7
        inc board = replace offset (board !! offset + 1) board

sow position stones board = sow (position + 1) (stones - 1)
  . sow position 1
  $ board

play :: Board -> Int -> Either Board String
play (playerBoard, opponentBoard) selectedHouse
  | selectedHouse < 0 || selectedHouse >= length playerBoard = Right "Invalid Move!"
  | playerBoard !! selectedHouse == 0 = Right "Invalid Move!"
  | otherwise = Left
    . sowRight selectedHouse playerBoard
    $ (replace selectedHouse 0 playerBoard, opponentBoard)
