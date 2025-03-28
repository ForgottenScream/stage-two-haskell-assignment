import Base
import Control.Concurrent
import DecisionTree
import Prelude

exampleBoard =
  [ Just One,
    Just Two,
    Just One,
    Nothing,
    Just One,
    Nothing,
    Just Two,
    Nothing,
    Just Two
  ]

-- Part 1 - Helper Functions

-- Helper Function: Player to String, useful for dialog message
playerToString :: Player -> String
playerToString One = "One"
playerToString Two = "Two"

letterToInt :: Char -> Int
letterToInt col =
  case col of
    'A' -> 0
    'a' -> 0
    'B' -> 1
    'b' -> 1
    'C' -> 2
    'c' -> 2

showBoard :: Board -> String
showBoard board = unlines [showRow i | i <- [0 .. 2]]
  where
    showRow i = concat [showCell (board !! (i * 3 + j)) | j <- [0 .. 2]]
    showCell Nothing = "_ "
    showCell (Just One) = "X "
    showCell (Just Two) = "O "

toPos :: (Int, Int) -> Maybe Int
toPos (x, y)
  | x < 0 || x > 2 || y < 0 || y > 2 = Nothing
  | otherwise = Just (x * 3 + y)

lookupBoard :: Board -> Int -> Maybe Player
lookupBoard board index
  | index < 0 || index >= length board = Nothing
  | otherwise = board !! index

addToGameBoard :: Board -> Int -> Player -> Maybe Board
addToGameBoard board index player
  | checkBoard = Just (addMove board index player)
  | otherwise = Nothing
  where
    checkBoard = lookupBoard board index == Nothing
    addMove board index player =
      take index board ++ [Just player] ++ drop (index + 1) board

checkWin :: Board -> Maybe Player
checkWin board =
  let winningCombinations =
        [ (0, 1, 2),
          (0, 3, 6),
          (0, 4, 8),
          (1, 4, 7),
          (2, 5, 8),
          (2, 3, 6),
          (3, 4, 5),
          (6, 7, 8)
        ]

      checkLine (a, b, c) =
        case (board !! a, board !! b, board !! c) of
          (Just p1, Just p2, Just p3) | p1 == p2 && p2 == p3 -> Just p1
          _ -> Nothing
  in foldr ( \combo acc -> maybe acc Just (checkLine combo)) Nothing winningCombinations

hline :: IO ()
hline = putStrLn (replicate 20 '-')

availableMoves :: Board -> Bool
availableMoves board = Nothing `elem` board

-- Part 2 Game Server and Co-ordination
select :: Player -> Chan Int -> Chan Int -> Chan Int
select player input1 input2 = if player == One then input1 else input2

writeChanTwice :: Chan a -> a -> IO ()
writeChanTwice chan msg = do
  writeChan chan msg
  writeChan chan msg

gameServer ::
  Player ->
  Board ->
  Chan Int ->
  Chan Int ->
  Chan Result ->
  IO (Maybe Player)
gameServer player boardState p1move p2move resultChan = do
  hline
  putStrLn ("Player " ++ playerToString player ++ ", enter your row (0-2) and then column (A-C):")

  rowMove <- readChan (select player p1move p2move)
  columnMove <- readChan (select player p1move p2move)

  -- Convert the row and column to a position
  let pos = toPos (rowMove, columnMove)

  case pos of
    Just index -> do
      putStrLn ("Player " ++ playerToString player ++ " attempted to move to " ++ show index)
      case addToGameBoard boardState index player of
        Just newBoard -> do
          putStrLn (showBoard newBoard)
          case checkWin newBoard of
            Just winner -> do
              writeChanTwice resultChan Win
              return (Just winner)
            Nothing -> do
              -- Check if there are available moves left
              if availableMoves newBoard
                then gameServer (flipPlayer player) newBoard p1move p2move resultChan
                else do
                  putStrLn "The game is a draw!"
                  writeChan resultChan Draw
                  return Nothing
        Nothing -> do
          -- Invalid move: position is already taken
          putStrLn "Invalid move! This position is already taken. The game continues."
          writeChanTwice resultChan Continue
          gameServer (flipPlayer player) boardState p1move p2move resultChan
    Nothing -> do
      -- Invalid move: position is out of bounds
      putStrLn "Invalid position! Please enter a valid row and column. The game continues."
      writeChanTwice resultChan Continue
      gameServer (flipPlayer player) boardState p1move p2move resultChan