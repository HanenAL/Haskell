module Expr where

import Data.Char
import Data.Maybe
import Test.QuickCheck

-----------------------------------------------------------------------------

data Expr = Num Double
          | Add Expr Expr
          | Mul Expr Expr
          | X
          | Sin Expr
          | Cos Expr

-----------------------------------------------------------------------------

showExpr :: Expr -> String
showExpr (Num n)   = show n
showExpr (Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Mul a b) = showFactor a ++ "*" ++ showFactor b
showExpr X         = "x"
showExpr (Sin n)   = "Sin " ++ "(" ++ showExpr n ++ ")"
showExpr (Cos n)   = "Cos " ++ "(" ++ showExpr n ++ ")"

showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++")"
showFactor e         = showExpr e

instance Show Expr where
  show = showExpr

-----------------------------------------------------------------------------

eval :: Expr -> Double -> Double
eval (Num n) x   = n
eval (Add a b) x = eval a x + eval b x
eval (Mul a b) x = eval a x * eval b x
eval X x         = x
eval (Sin n) x   = sin (eval n x)
eval (Cos n) x   = cos (eval n x)

-----------------------------------------------------------------------------

type Parser a = String -> Maybe (a,String)

-- `number` parses a number
number :: Parser Double -- )
number s = listToMaybe(reads s)
--number ('-':s)           = fmap negate' (number s)

negate' :: (Double,String) -> (Double,String)
negate' (n,s) = (-n,s)

-- `num` parses a numeric expression
num :: Parser Expr
num s = case number s of
    Just (n,s') -> Just (Num n, s')
    Nothing     -> Nothing

-- * an expression is a '+'-chain of terms
-- * a term is a '*'-chain of factors
expr, term :: Parser Expr
expr = chain term   '+' Add
term = chain factor '*' Mul

-- `chain p op f s1` parsers a "chain" of things.
--
--   * The things are parsed by the parser `p`.
--   * The things are separated by the symbol `op`.
--   * The things are combined by the function `f`.
--
-- For example "12+23+1+172" is a chain of numbers, separated by the symbol '+'.
-- chain string int "+" Add
chain :: Parser a -> Char -> (a -> a -> a) -> Parser a
chain p op f s1 =
  case p s1 of
    Just (a,s2) -> case s2 of
                     c:s3 | c == op -> case chain p op f s3 of
                                         Just (b,s4) -> Just (f a b, s4)
                                         Nothing     -> Just (a,s2)
                     _              -> Just (a,s2)
    Nothing     -> Nothing

-- `factor` parses a "factor": either a number or an expression surrounded by
-- parentheses
factor :: Parser Expr
factor ('(  ':s) =
   case expr s of
      Just (a, ')':s1) -> Just (a, s1)
      _                -> Nothing
factor s = num s



readExpr :: String -> Maybe Expr
readExpr s =
  case expr s of
    Just (a,"") -> Just a
    _           -> Nothing

-----------------------------------------------------------------------------