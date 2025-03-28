import Base
import Control.Concurrent
import DecisionTree

-- Part 1 - Helper Functions

-- Helper Function: Player to String, useful for dialog message
playerToString :: Player -> String
playerToString One = "One"
playerToString Two = "Two"

-- Print the current state of the board
showBoard :: Board -> String
showBoard board = unlines [showRow i | i <- [0 .. 2]]
  where
    showRow i = concat [showCell (board !! (i * 3 + j)) | j <- [0 .. 2]]
    showCell Nothing = "_ "
    showCell (Just One) = "X "
    showCell (Just Two) = "O "

-- Calculate the position on the board based on the coordinates given.
toPos :: (Int, Int) -> Maybe Int
toPos (x, y)
  | x < 0 || x > 2 || y < 0 || y > 2 = Nothing
  | otherwise = Just (x * 3 + y)

-- Look up which player (if any) played on the specific position
lookupBoard :: Board -> Int -> Maybe Player
lookupBoard board index
  | index < 0 || index >= length board = Nothing
  | otherwise = board !! index

-- Update the board with a players' move
addToGameBoard :: Board -> Int -> Player -> Maybe Board
addToGameBoard board index player
  | checkBoard = Just (addMove board index player)
  | otherwise = Nothing
  where
    checkBoard = lookupBoard board index == Nothing
    addMove board index player =
      take index board ++ [Just player] ++ drop (index + 1) board

-- Check if any player has won the game
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

-- Create a simple seperator.
hline :: IO ()
hline = putStrLn (replicate 20 '-')

-- Check if there are any available moves left on the board.
availableMoves :: Board -> Bool
availableMoves board = Nothing `elem` board

-- Part 2 Game Server and Co-ordination

-- Useful for alternating which player to receive a move from during the game.
select :: Player -> Chan Int -> Chan Int -> Chan Int
select player input1 input2 = if player == One then input1 else input2

-- Write to both players' channels.
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
  putStrLn $ "Player " ++ playerToString player ++ ", enter your row (0-2) and then column (A-C):"

  -- Read the move directly as an Int
  index <- readChan (select player p1move p2move)

  putStrLn $ "Player " ++ playerToString player ++ " attempted to move to " ++ show index
  case addToGameBoard boardState index player of
    Just newBoard -> do
      putStrLn $ showBoard newBoard
      case checkWin newBoard of
        Just winner -> do
          putStrLn $ "Player " ++ playerToString player ++ " Wins!"
          writeChanTwice resultChan (Win winner newBoard)
          return (Just winner)
        Nothing -> do
          if availableMoves newBoard
            then do
              putStrLn "The game continues."
              writeChanTwice resultChan (Continue newBoard)
              gameServer (flipPlayer player) newBoard p1move p2move resultChan
            else do
              putStrLn "The game is a draw!"
              writeChanTwice resultChan (Draw newBoard)
              return Nothing
    Nothing -> do
      putStrLn "Invalid move! This position is already taken. The game continues."
      writeChanTwice resultChan (Continue boardState)
      gameServer (flipPlayer player) boardState p1move p2move resultChan

-- This function will start the game server and handle whether another game will be played.
gameServerStart :: Player
              -> (Chan Coordination, Chan Coordination)
              -> (Chan Int, Chan Int)
              -> (Int, Int)
              -> Chan Result -> IO ()
gameServerStart startingPlayer (coordinatorP1, coordinatorP2) (p1move, p2move) (scoreP1, scoreP2) resultChan = do
  let initialBoard = replicate 9 Nothing

  result <- gameServer startingPlayer initialBoard p1move p2move resultChan
  
  putStrLn $ "Score Board: Player One = " ++ show scoreP1 ++ ", Player Two = " ++ show scoreP2

  putStrLn "Play Again? Player One needs to say Y/N"
  response <- getLine

  if response == "Y" then do
    writeChan coordinatorP1 Again
    writeChan coordinatorP2 Again
    gameServerStart (flipPlayer startingPlayer) (coordinatorP1, coordinatorP2) (p1move, p2move) (scoreP1, scoreP2) resultChan
  else do
    writeChan coordinatorP1 Stop
    writeChan coordinatorP2 Stop
    putStrLn "End of tournament!"

-- This function will start the game with the two players and the required processes.
startGame :: (Player -> Chan Coordination -> Chan Int -> Chan Result -> IO ())
          -> (Player -> Chan Coordination -> Chan Int -> Chan Result -> IO ())
          -> IO ()
startGame player1 player2 = do

  -- creating channels for everything
  coordinatorP1 <- newChan
  coordinatorP2 <- newChan
  p1move <- newChan
  p2move <- newChan
  resultChan <- newChan
  
  -- starting concurrent processes
  _ <- forkIO $ player1 One coordinatorP1 p1move resultChan
  _ <- forkIO $ player2 Two coordinatorP2 p2move resultChan

  gameServerStart One (coordinatorP1, coordinatorP2) (p1move, p2move) (0,0) resultChan

parseInput :: String -> String -> Maybe Int
parseInput rowString colString = do
  row <- parseRow rowString
  col <- parseCol colString
  toPos (row, col)
  where
    parseRow :: String -> Maybe Int
    parseRow "1" = Just 0
    parseRow "2" = Just 1
    parseRow "3" = Just 2
    parseRow "_" = Nothing
    
    parseCol :: String -> Maybe Int
    parseCol "A" = Just 0
    parseCol "B" = Just 1
    parseCol "C" = Just 2
    parseCol "a" = Just 0
    parseCol "b" = Just 1
    parseCol "c" = Just 2
    parseCol _ = Nothing

humanPlayer :: Player -> Chan Coordination -> Chan Int -> Chan Result -> IO ()
humanPlayer player coordinator moves resultChan = do
  -- start the game loop
  humanPlayerGame player moves resultChan coordinator

humanPlayerGame :: Player -> Chan Int -> Chan Result -> Chan Coordination -> IO ()
humanPlayerGame player moves resultChan coordinator = do
  result <- readChan resultChan --this should read the result from the game server
  case result of
    Win winner _ -> do
      putStrLn $ "Player " ++ playerToString winner ++ " has won the game!"
      handlePostGame player coordinator
    Draw _ -> do
      putStrLn "This game is a draw."
      handlePostGame player coordinator
    Continue board -> do
      putStrLn $ showBoard board

      rowInput <- getLine
      colInput <- getLine
      
      case parseInput rowInput colInput of
        Just index -> do
          writeChan moves index
          humanPlayerGame player moves resultChan coordinator

handlePostGame :: Player -> Chan Coordination -> IO ()
handlePostGame player coordinator = do
  if player == One
    then do
      putStrLn "Player Again? Y/N"
      response <- getLine
      if response == "Y"
        then writeChan coordinator Again
        else writeChan coordinator Stop
  else do
    coordinationMessage <- readChan coordinator
    case coordinationMessage of
      Again -> do
        putStrLn "Player One has chosen to play again."
      Stop -> do
        putStrLn "Player One has chosen to stop the tournament."

dummyAIPlayer :: Player -> Chan Coordination -> Chan Int -> Chan Result -> IO ()
dummyAIPlayer player coordinator moves resultChan = do
  result <- readChan resultChan
  case result of
    Win winner _ -> putStrLn $ "Player " ++ playerToString winner ++ " has won the game!"
    Draw _ -> putStrLn "This game is a draw."
    Continue board -> do
      putStrLn $ showBoard board
      let index = 0  -- Always try to play in the top-left corner
      if lookupBoard board index == Nothing  -- Check if the position is available
        then do
          writeChan moves index
          dummyAIPlayer player coordinator moves resultChan
        else do
          putStrLn "Dummy AI cannot play, position is already taken."
          -- Let the game continue without making a move
          dummyAIPlayer player coordinator moves resultChan