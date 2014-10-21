module ExprQC where

import Data.Char
import Data.Maybe
import Test.QuickCheck

import Expr

-----------------------------------------------------------------------------

prop_ShowReadExpr :: Expr -> Bool
prop_ShowReadExpr expr = (eval (fromJust (readExpr $ showExpr expr)) 0) `almostEqual` (eval expr  0)

almostEqual :: Double -> Double -> Bool
almostEqual x y = (x - y) <= 0.001

instance Arbitrary Expr where
  arbitrary = sized arbExpr

arbExpr :: Int -> Gen Expr
arbExpr s =
  frequency [ (1, do n <- arbitrary
                     return (Num n))
            , (s, do a <- arbExpr s'
                     b <- arbExpr s'
                     return (Add a b))
            , (s, do a <- arbExpr s'
                     b <- arbExpr s'
                     return (Mul a b))
            , (s, do n <- arbExpr s'
                     return (Sin n))
            , (s, do n <- arbExpr s'
                     return (Cos n))
            , (s, return X)
            ]
 where
  s' = s `div` 2

-----------------------------------------------------------------------------