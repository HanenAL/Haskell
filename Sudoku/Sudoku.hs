data Sudoku = Sudoku [[Maybe Int]]

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

-- A1:
-- Creates a list of lists containing 9 "nothing" each, (map).
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku ([ [ Nothing | x <- [1..9] ] | x <- [1..9] ])

-- A2:
isSudoku :: Sudoku -> Bool
isSudoku sudoku = 
