-- Victor Longberg Haskell lab 3 viclon-8@student.ltu.se
module Expr(Expr, T, parse, fromString, value, toString) where

{-

   An expression of type Expr is a representation of an arithmetic expression 
   with integer constants and variables. A variable is a string of upper- 
   and lower case letters. The following functions are exported
   
   parse :: Parser Expr
   fromString :: String -> Expr
   toString :: Expr -> String
   value :: Expr -> Dictionary.T String Int -> Int
   
   parse is a parser for expressions as defined by the module Parser.
   It is suitable for use in parsers for languages containing expressions
   as a sublanguage.
   
   fromString expects its argument to contain an expression and returns the 
   corresponding Expr. 
  
   toString converts an expression to a string without unneccessary 
   parentheses and such that fromString (toString e) = e.
  
   value e env evaluates e in an environment env that is represented by a
   Dictionary.T Int.  

-}

import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary

data Expr = Num Integer | Var String | Add Expr Expr 
       | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
         deriving Show

type T = Expr

var, num, factor, term, expr :: Parser Expr

term', expr' :: Expr -> Parser Expr

var = word >-> Var

num = number >-> Num

mulOp = lit '*' >-> const Mul !
        lit '/' >-> const Div

addOp = lit '+' >-> const Add !
        lit '-' >-> const Sub

bldOp e (oper,e') = oper e e'

factor = num !
         var !
         lit '(' -# expr #- lit ')' !
         err "illegal factor"
             
term' e = mulOp # factor >-> bldOp e #> term' ! return e
term = factor #> term'
       
expr' e = addOp # term >-> bldOp e #> expr' ! return e
expr = term #> expr'

parens :: Bool -> [Char] -> [Char]
parens cond str = if cond then "(" ++ str ++ ")" else str

shw :: Int -> Expr -> String
shw prec (Num n) = show n  -- shw 1 (Num 2)
shw prec (Var v) = v       -- shw 1 (Var "test" )
shw prec (Add t u) = parens (prec>5) (shw 5 t ++ "+" ++ shw 5 u) -- shw 1 (Add (Num 12) (Num 34) )
shw prec (Sub t u) = parens (prec>5) (shw 5 t ++ "-" ++ shw 6 u) -- shw 1 (Sub (Num 12) (Num 34) )
shw prec (Mul t u) = parens (prec>6) (shw 6 t ++ "*" ++ shw 6 u) -- shw 1 (Mul (Num 12) (Num 34) )
shw prec (Div t u) = parens (prec>6) (shw 6 t ++ "/" ++ shw 7 u) -- shw 1 (Div (Num 12) (Num 34) )

value :: Expr -> Dictionary.T String Integer -> Integer
value (Num n) _ = n
value (Var x) env = case Dictionary.lookup x env of Just y -> y ; _ -> error (x ++ " undefined") -- Lite oklart ??
value (Add t u) env = (+) (value t env) (value u env)
value (Sub t u) env = (-) (value t env) (value u env)
value (Mul t u) env = (*) (value t env) (value u env)
value (Div t u) env
        | value u env == 0      = error "division by 0"
        | otherwise             = div (value t env) (value u env)

{-
--Part 2 // Task 1 - Addning support for sin cos log and exp.
eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined") -- Lite oklart
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
-}

instance Parse Expr where
    parse = expr
    toString = shw 0
