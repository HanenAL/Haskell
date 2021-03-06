module Main where

import Data.Char
import Data.List
import Data.Maybe
import Test.QuickCheck

-----------------------------------------------------------------------------

data Sudoku = Sudoku [[Maybe Int]]
   deriving(Eq, Show)

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

example :: Sudoku
example =
    Sudoku
      [ [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5]
      , [Just 3, Just 6, Just 5,Just 5,Nothing, Just 1, Just 2, Just 5,Just 5]
      , [Just 3, Just 6, Nothing,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5]
      , [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5]
      , [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5]
      , [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5]
      , [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5]
      , [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5]
      , [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5]
      ]

example2 :: Sudoku
example2 =
    Sudoku
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]

-----------------------------------------------------------------------------

-- A1:
-- Creates a list of lists containing 9 "nothing" each, (map).
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku ([ [ Nothing | x <- [1..9] ] | x <- [1..9] ])

-- A2:
-- Checks if the sudoku is in okay format
isSudoku :: Sudoku -> Bool
isSudoku sudoku = sizeOK (rows sudoku)

-- Checks size of all the rows and the list of rows
sizeOK :: [[Maybe Int]] -> Bool
sizeOK xs | size xs /= 9 = False
sizeOK xs | otherwise    = and [ (size (selectRow xs x )) == 9 | x <- [0..8] ]

selectRow :: [[a]] -> Int -> [a]
selectRow (x:_) 0  = x
selectRow (x:xs) n = selectRow xs (n-1)

checkRow :: [Maybe Int] -> Bool
checkRow row = and [ Nothing /= x | x <- row ]

size :: [a] -> Int
size []     = 0
size (x:xs) = 1 + size xs

-- A3:
isSolved :: Sudoku -> Bool
isSolved sudoku = and [ checkRow (selectRow (rows sudoku) x) | x <- [0..8] ]

-----------------------------------------------------------------------------

-- B1:
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = putStr $ printLines(sudokuToString sudoku)

-- Combine all the strings into one string on new lines.
printLines :: [String] -> String
printLines []   = ""
printLines (x:xs) = x ++ "\n" ++ printLines xs

-- Convert a complete sudoku to a list of strings.
sudokuToString :: Sudoku -> [String]
sudokuToString sudoku = [ printRow (selectRow(rows sudoku) x) | x <- [0..8] ]

-- Converts a single row into a string.
printRow :: [Maybe Int] -> String
printRow []           = ""
printRow (Nothing:xs) = "." ++ printRow xs
printRow (Just a:xs)  = [intToDigit a] ++ printRow xs

-- B2:
readSudoku :: FilePath -> IO Sudoku
readSudoku file =
   do string <- readFile file
      return(makeSudoku string)

-- Removes new lines and creates a list of all the rows as strings.
listRows :: String -> [String]
listRows s = lines s

-- converts a string into a row.
convert :: String -> [Maybe Int]
convert []                 = []
convert ('.':xs)           = [Nothing] ++ convert xs
convert (x:xs) | isDigit x = [Just (digitToInt x)] ++ convert xs
               | otherwise = error "Not a sudoku!"

-- With the use of convert, creates a sudoku out of the strings.
makeSudoku :: String -> Sudoku
makeSudoku xs = Sudoku [ convert s | s <- ys]
   where ys = listRows xs

-----------------------------------------------------------------------------

-- C1:
-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency
     [ (9, return Nothing)
     , (1, do a <- choose (1,9)
              return (Just a))
     ]

-- C2:
-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-- C3:
-- Is the random generated object a sudoku?
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudoku = isSudoku sudoku

-----------------------------------------------------------------------------

type Block = [Maybe Int]

-- D1:
isOkayBlock :: Block -> Bool
isOkayBlock block = nub (removeNothing block) == removeNothing block

-- Removes *Nothing*
removeNothing :: Block -> Block
removeNothing []           = []
removeNothing (Nothing:xs) = removeNothing xs
removeNothing (x:xs)       = [x] ++ removeNothing xs

-- D2
blocks :: Sudoku -> [Block]
blocks sud = (rows sud) ++ (transpose (rows sud)) ++ (blocks' sud)

blocks' :: Sudoku -> [Block]
blocks' (Sudoku []) = []
blocks' sud@(Sudoku (x:y:z:xs)) 
               | z == []   = blocks' (Sudoku xs)
               | otherwise = [createBlock sud 3] ++ 
                 blocks' (Sudoku ((drop 3 x):(drop 3 y):(drop 3 z):xs))

-- Creates a single block
createBlock :: Sudoku -> Int -> Block
createBlock (Sudoku (x:xs)) 0 = []
createBlock (Sudoku (x:[])) n = take 3 x
createBlock (Sudoku (x:xs)) n = take 3 x ++ createBlock (Sudoku xs) (n - 1)


prop_blocks :: Sudoku -> Bool
prop_blocks sud = length (blocks sud) == 27 && prop_blocks' (blocks sud) == 243

prop_blocks' :: [Block] -> Int
prop_blocks' [] = 0
prop_blocks' (x:xs) = (length x) + prop_blocks' xs


-- D3
isOkay :: Sudoku -> Bool	
isOkay sud = and [ isOkayBlock x | x <- (blocks sud) ]

-----------------------------------------------------------------------------

type Pos = (Int, Int)

-- E1
blank :: Sudoku -> Pos
blank (Sudoku rs) | n > 8     = (9, 9)
                  | otherwise = (n, findBlankInRow(selectRow rs n))
   where n = findBlankInColumn rs

findBlankInRow :: Block -> Int
findBlankInRow []                     = 0
findBlankInRow (x:xs)
                       | x /= Nothing = 1 + findBlankInRow xs
                       | otherwise    = 0
                       
findBlankInColumn :: [Block] -> Int
findBlankInColumn []     = 0
findBlankInColumn (x:xs) | findBlankInRow x > 8 = 1 + findBlankInColumn xs
                         | otherwise            = 0

prop_isBlank :: Sudoku -> Bool
prop_isBlank sud = (selectEle (selectRow (rows sud) x) y) == Nothing
   where (x, y) = blank sud

selectEle :: [a] -> Int -> a
selectEle (x:_) 0  = x
selectEle (x:xs) n = selectEle xs (n-1)

-- E2
(!!=) :: [a] -> (Int, a) -> [a]
[]     !!= (index, obj)              = [obj]
(x:xs) !!= (index, obj) | index /= 0 = x : (xs) !!= (index - 1, obj)
                        | otherwise  = obj : xs

prop_changeElem :: [Int] -> (Int,Int) -> Property
prop_changeElem list (index, obj) = list /= [] ==> (list !!= (index', obj)) !! index' == obj
  where
    index' = index `mod` length list

-- E3
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update (Sudoku rs) (y, x) obj = Sudoku (rs !!= (y, (updateRow rs (y, x) obj)))

updateRow :: [Block] -> Pos -> Maybe Int -> Block
updateRow rs (y, x) obj = (selectRow rs y) !!= (x, obj)

prop_updatePosition :: Sudoku -> Pos -> Maybe Int -> Bool
prop_updatePosition sud (y, x) obj = (selectRow (rows (update sud (y', x') obj)) y') !! x' == obj
  where
    y' = y `mod` 9
    x' = x `mod` 9

prop_updateChanged :: Sudoku -> Pos -> Maybe Int -> Property
prop_updateChanged sud pos@(y,x) obj = obj /= Nothing ==> (update sud pos obj) /= sud

-------------------------------------------------------------------------------------------

-- F1:
solve :: Sudoku -> Maybe Sudoku
solve sud | isOkay sud = Just $ head $ checkSudokus (addSudoku sud (blank sud) 9)
          | otherwise  = Nothing

addSudoku :: Sudoku -> Pos -> Int -> [Sudoku] 
addSudoku sud pos@(y, x) 0 = []
addSudoku sud pos@(y, x) n | isOkay (update sud pos (Just n)) =
                       update sud pos (Just n) : addSudoku sud pos (n - 1)
                           | otherwise = addSudoku sud pos (n - 1)
                          
checkSudokus :: [Sudoku] -> [Sudoku]
checkSudokus sud | or[ isSolved x | x <- sud ] = sud
checkSudokus sud = checkSudokus (concat [addSudoku a (blank a) 9 | a <- sud])

main :: IO()
main = do 
         sud <- readSudoku "hard55.sud"
         printSudoku $ head $ checkSudokus (addSudoku sud (blank sud) 9)

-- F2:
readAndSolve :: FilePath -> IO()
readAndSolve path = do 
  sud <- readSudoku path
  if solve sud == Nothing
  then putStr "No solution\n"
  else printSudoku $ fromJust $ solve sud 
         
-- F3:
isSolutionOf :: Sudoku -> Sudoku -> Bool
sud1 `isSolutionOf` sud2 = isOkay sud1 && sud1 == fromJust (solve sud2)
                         
--F4
fewerCheck prop = quickCheckWith stdArgs{ maxSuccess = 5 } prop

prop_SolveSound :: Sudoku -> Property
prop_SolveSound sud = 
  isOkay sud ==> (fromJust $ solve sud) `isSolutionOf` sud

---------------------------------------------------------------------------------------------