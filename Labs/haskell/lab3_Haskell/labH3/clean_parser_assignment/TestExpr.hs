-- Victor Longberg Haskell lab 3 viclon-8@student.ltu.se

{- Test for Expr-}
module TestExpr where

import qualified Dictionary
import Expr

dict = Dictionary.insert ("x", 1) $
       Dictionary.insert ("y", 2) 
       Dictionary.empty 

testValue string = value (fromString string) dict

n1 = testValue "1"
n2 = testValue "x"
n3 = testValue "x+y"
n4 = testValue "x-y-y"
n21 = testValue "1/(2-y)" {-  Expr.value: division by 0 -}
n31 = testValue "2+z"     {-  Expr.value: undefined variable z -}

main :: IO()
main = do
       print $ testValue "1"
       print $ testValue "x"
       print $ testValue "x+y"
       print $ testValue "x-y-y"
       print $ testValue "1/(2-y)"
       print $ testValue "2+z" 