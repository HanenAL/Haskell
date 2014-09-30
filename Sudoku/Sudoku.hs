module Sudoku where
import Data.Char
import Data.List
import Test.QuickCheck

--import Test.QuickCheck

-----------------------------------------------------------------------------

data Sudoku = Sudoku [[Maybe Int]]
   deriving(Eq, Show)

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

example :: Sudoku
example =
    Sudoku
      [ [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5]
      , [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5]
      , [Just 3, Just 6, Just 5,Just 5,Just 7, Just 1, Just 2, Just 5,Just 5]
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

selectRow :: [[Maybe Int]] -> Int -> [Maybe Int]
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

-- Add function: check every row if they have any "nothing".
-- Add function: Adds the boolean value of all rows.

-----------------------------------------------------------------------------

-- printRow (selectRow(rows example2) 0)
-- B1:
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = putStr $ printLines(sudokuToString sudoku)

printLines :: [String] -> String
printLines []   = ""
printLines (x:xs) = x ++ "\n" ++ printLines xs

sudokuToString :: Sudoku -> [String]
sudokuToString sudoku = [ printRow (selectRow(rows sudoku) x) | x <- [0..8] ]

printRow :: [Maybe Int] -> String
printRow []           = ""
printRow (Nothing:xs) = "." ++ printRow xs
printRow (Just a:xs)  = [intToDigit a] ++ printRow xs

-- B2:
readSudoku :: FilePath -> IO Sudoku
readSudoku file =
   do s <- readFile file
      return(makeSudoku s)

listRows :: String -> [String]
listRows s = lines s

convert :: String -> [Maybe Int]
convert []                         = []
convert ('.':xs)                   = [Nothing] ++ convert xs
convert (x:xs) | isDigit x == True = [Just (digitToInt x)] ++ convert xs
               | otherwise         = error "Not a sudoku!"

makeSudoku :: String -> Sudoku
makeSudoku xs = Sudoku [ convert s | s <- ys]
   where ys = listRows xs

-----------------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = undefined
--cell = frequency
--     [ (1, Nothing)
--     , (9, do a <- choose (1,9)
--              return (a)) -- Needs to be a INT. How?
--     ]

-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)

-----------------------------------------------------------------------------
