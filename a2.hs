import Base
import Control.Concurrent
import DecisionTree

showBoard :: Board -> String
showBoard board = buildString board "" 0
  where
    buildString [] acc _ = acc
    buildString (x : xs) acc count =
      let newAcc = acc ++ formatCell x
          newCount = count + 1
          accWithNewLine =
            if newCount `mod` 3 == 0 && not (null xs)
              then newAcc ++ "\n"
              else newAcc
       in buildString xs accWithNewLine newCount
    formatCell :: Maybe Player -> String
    formatCell Nothing = "_ "
    formatCell (Just One) = "X "
    formatCell (Just Two) = "O "

exampleBoard :: Board
exampleBoard =
  [ Just One,
    Nothing,
    Just Two,
    Nothing,
    Nothing,
    Nothing,
    Nothing,
    Nothing,
    Just One
  ]
