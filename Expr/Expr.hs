module Expr where

import Data.Char
import Data.Maybe

-----------------------------------------------------------------------------

data Expr = Num Double
          | Add Expr Expr
          | Mul Expr Expr
          | X
          | Sin Expr
          | Cos Expr
    deriving(Eq)

example :: Expr
example = (Sin (Add (Num 3) (Mul (Num 2) (X))))

-----------------------------------------------------------------------------

showExpr :: Expr -> String
showExpr (Num n)   = show n
showExpr (Add a b) = showExpr a ++ "+" ++ showExpr b
showExpr (Mul a b) = showFactor a ++ "*" ++ showFactor b
showExpr X         = "x"
showExpr (Sin n)   = "sin " ++ showSC n
showExpr (Cos n)   = "cos " ++ showSC n

showFactor :: Expr -> String
showFactor (Add a b) = "(" ++ showExpr (Add a b) ++")"
showFactor e         = showExpr e

showSC :: Expr -> String
showSC X       = "x"
showSC (Num n) = show n
showSC x       = "(" ++ showExpr x ++ ")"

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

number :: Parser Double
number ('-':s) = fmap negate' (number s)
number s = listToMaybe(reads s)

negate' :: (Double,String) -> (Double,String)
negate' (n,s) = (-n,s)

num :: Parser Expr
num s = case number s of
    Just (n,s') -> Just (Num n, s')
    Nothing     -> Nothing

expr, term :: Parser Expr
expr = chain term   '+' Add
term = chain factor '*' Mul

chain :: Parser a -> Char -> (a -> a -> a) -> Parser a
chain p op f s1 =
  case p s1 of
    Just (a,s2) -> case s2 of
                     c:s3 | c == op -> case chain p op f s3 of
                                         Just (b,s4) -> Just (f a b, s4)
                                         Nothing     -> Just (a,s2)
                     _              -> Just (a,s2)
    Nothing     -> Nothing

factor :: Parser Expr
factor ('(':s) =
   case expr s of
      Just (a, ')':s1) -> Just (a, s1)
      _                -> Nothing
factor ('s':'i':'n':s) =
   case factor s of
      Just (a, s1) -> Just (Sin a, s1)
      _            -> Nothing
factor ('c':'o':'s':s) =
   case factor s of
      Just (a, s1) -> Just (Cos a, s1)
      _            -> Nothing
factor ('x':s) = Just (X, s)
--   case factor s of
--      Just (a, s1) -> Just (X, s1)
--      _            -> Nothing
factor s = num s

readExpr :: String -> Maybe Expr
readExpr s =
  case expr s' of
    Just (a,"") -> Just a
    _           -> Nothing
  where
    s' = filter (/= ' ') s

-----------------------------------------------------------------------------
