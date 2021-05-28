module Main where
import Data.List(intercalate)
import Data
import Lib

main :: IO ()
main = do
    putStrLn $  intercalate "\n" (parseToShow createInitialGrid)
    og $ availableWords

parseToShow :: Grid -> [String]
parseToShow (Grid []) = []
parseToShow (Grid (l:ls)) = map letter l : parseToShow (Grid ls)

