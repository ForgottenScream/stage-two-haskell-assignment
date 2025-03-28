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
   in foldr
        ( \combo acc ->
            case checkLine combo of
              Just winner -> Just winner
              Nothing -> acc
        )
        Nothing
        winningCombinations

hline :: IO ()
hline = putStrLn (replicate 20 '-')

-- Part 2 Game Server and Co-ordination
-- select :: Player -> Chan Int -> Chan Int -> Chan Int
