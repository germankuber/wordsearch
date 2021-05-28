module Lib  where
import Data.List (transpose, find)
import Data.Char (toLower)
import Data

data Cell = Cell {
    position :: (Int, Int),
    letter :: Char
 }  |  EmptyCell deriving (Show)


newtype Grid = Grid [[Cell]] deriving (Show)

listToLower :: [String] -> [String]
listToLower = map (map toLower)


stretchLines :: Grid -> Grid
stretchLines grid = Grid (stretchAllLines [] grid)  where
    stretchAllLines :: [Cell] -> Grid -> [[Cell]]
    stretchAllLines _ (Grid []) = []
    stretchAllLines c (Grid (l:ls)) = (c ++ l) : stretchAllLines (EmptyCell : c) (Grid ls)


parseGridToList :: Grid -> [String]
parseGridToList (Grid []) = []
parseGridToList (Grid (l:ls)) = map letter l : parseGridToList (Grid ls)


getLines :: Grid -> Grid
getLines grid@(Grid cells) =
    let (Grid listOfCells) = stretchLines grid
        listTranspose = transpose cells
        listTransposeReverse = map reverse $ transpose cells
        listReverse = map reverse listOfCells
        (Grid listOfCellsToTranspose) = stretchLines (Grid   (map reverse cells))
        diagonalReverse =   transpose$ listOfCellsToTranspose
        diagonal = transpose $ listOfCells
    in Grid (listOfCells ++ listTranspose ++ listTransposeReverse ++ listReverse ++ diagonal ++ diagonalReverse)


searchWordInGrid :: String -> Grid -> Maybe String
searchWordInGrid word grid = result where
    searchWordInAllPlaces = do foundWordInListToSearch <- searchWordInList word availableWords
                               searchWord word (getLines grid)
    result = searchWordInAllPlaces


searchWordInList :: (Foldable t, Eq a) => a -> t a -> Maybe a
searchWordInList word = find (==word)


searchWord :: String -> Grid -> Maybe String
searchWord _ (Grid []) = Nothing
searchWord w (Grid (l:ls)) | searchWordInLine w w l = Just w
                           | otherwise = searchWord w (Grid ls)



searchWordInLine :: String -> String -> [Cell] -> Bool
searchWordInLine _ [] [] = True
searchWordInLine _ [] _ = True
searchWordInLine _ _ [] = False
searchWordInLine wordC (w:word) (l:line) =
    case l of
        EmptyCell -> searchWordInLine wordC wordC line
        (Cell _ l) ->
            let function | w == l = searchWordInLine wordC word line
                         | otherwise  = searchWordInLine wordC wordC line
            in function


coordinates :: [[(Int, Int)]]
coordinates = do
  let rowMax = length $ head $ gridOfWords
  row <- [1 .. rowMax]
  return $ do
    col <- [1 .. rowMax]
    return (row, col)


createInitialGrid :: Grid
createInitialGrid = Grid (calculateGrid coordinates gridOfWords)  where
    calculateGrid [] [] = []
    calculateGrid (c:cs) (w:ws) =
        let result = zipWith Cell  c w
        in result : calculateGrid cs ws


og :: Show a => [a] -> IO ()
og = putStrLn . unlines . map show

showGrid (Grid c) = og  c