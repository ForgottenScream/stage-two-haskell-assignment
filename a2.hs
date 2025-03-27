import Base
import Control.Concurrent
import DecisionTree
import Prelude

showBoard :: Board -> IO ()
showBoard board = putStrLn (unlines [showRow [board !! (i * 3 + j) | j <- [0 .. 2]] | i <- [0 .. 2]])
  where
    showRow :: [Maybe Player] -> String
    showRow row = unwords [showCell cell | cell <- row]
    showCell :: Maybe Player -> String
    showCell Nothing = "_"
    showCell (Just One) = "X"
    showCell (Just Two) = "O"

toPos :: (Int, Int) -> Maybe Int
toPos (x, y)
  | x < 0 || x > 2 || y < 0 || y > 2 = Nothing
  | otherwise = Just (3 * x + y)

-- lookupBoard :: Board -> Int -> Maybe Player
