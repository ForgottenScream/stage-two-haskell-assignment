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

-- lookupBoard :: Board -> Int -> Maybe Player
