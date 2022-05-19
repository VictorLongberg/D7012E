-- Code to Haskell lab assignment 2 in the course D7012E by Håkan Jonsson

-- Lab assigment 2 in the course D7012E done by Victor Longberg / viclon-8@student.ltu.se

module Expr where

import Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show) 

-- https://stackoverflow.com/questions/3864647/how-does-deriving-work-in-haskell
-- derivning allows for creating your own derivning methods "magic"?

-- No changes made here.
parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)
    
    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)
    
    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)
    
    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op [y] term term1, zs)
          where
            (term1,zs) = buildterm ys
    
    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)  
        accfactors (fact, y:ys) = (Op [y] fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys
    
    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"

-- Added unparsing for App
unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
unparse (App func arg) =  func ++ "(" ++ unparse arg  ++ ")" -- func has to be outside of the unprase arg.

--Part 2 // Task 1 - Addning support for sin cos log and exp.
eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined") -- Lite oklart
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
-- Part 2 // Task 1: Parsing: Sin, Cos, Log, Exp.
eval (App "sin" arg) env = sin $ eval arg env
eval (App "cos" arg) env = cos $ eval arg env
eval (App "log" arg) env = log $ eval arg env
eval (App "exp" arg) env = exp $ eval arg env

diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) = Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) = Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)
-- Part 2 // Task 1: Parsing sin, cos, log, exp
-- Example derivates: 
--           sin -> cos -> -sin -> -cos
--           log(x⁵) = 5x⁴/x⁵ --> 5/x
--           Exp(x) = e^(5x) -> 5*e^(5x)
diff v (App "sin" arg) = Op "*" (diff v arg) $ App "cos" arg 
diff v (App "cos" arg) = Op "*" (diff v arg) $ Op "*" (Var "0-1")(App "sin" arg)    -- Weird "bugg" but parse cant handle (-1) 
--diff v (App "cos" arg) = Op "*" (diff v arg) $ Op "*" (Const (-1))(App "sin" arg) -- so i made it to (0-1) and it works?
diff v (App "log" arg) = Op "/" (diff v arg) arg
diff v (App "exp" arg) = Op "*" (diff v arg) $ App "exp" arg
diff _ _ = error "can not compute the derivative"

-- Part 2 // Task 1: Added support to simplify sin,cos,log,exp
simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re
      (op,le,re)      -> Op op le re
simplify (App func arg) =  -- Copy and paste från Op
  let args = simplify arg in
    case (func, arg) of 
      ("sin",e)       -> App "sin" e
      ("cos",e)       -> App "cos" e
      ("log",e)       -> App "log" e
      ("exp",e)       -> App "exp" e

-- Part 3 // Task 2: Creating Functions
mkfun :: (EXPR, EXPR) -> (Float -> Float) -- Requires Number input?
mkfun (body, var) x = eval body [(unparse var, x)]

-- Reading instructions stepwise.
-- mkfun (body,var) = \x -> eval body  -- Eval needs [(String,Float)]
-- mkfun (body,var) = \x -> eval body [(String,x)] --  String -> EXPR (unparse)
-- mkfun (body,var) = \x -> eval body [(unparse var, x)] -- Compiler says to remove lambda.

-- 4 Newton-Raphson
-- We want to run Newton-Raphson for xn untill the difference between 
-- pn1 and pn is 0.0001 while only calculating the derivatie once.

-- Var:   f:               f':                                          x0:  
-- "x"    "x*x*x+x-1"      "((((x+x)*x)+(x*x))+1))"                     1.0
-- "y"    "cos(y)*sin(y)"  "(((0-1*sin(y))*sin(y))+(cos(y)*cos(y)))"    2.0
findZero :: String -> String -> Float -> Float
findZero s1 s2 =  findHelp fx f'x
  where
    f   = s2
    f'  = unparse $ simplify (diff (Var s1) (parse s2))
    fx  = mkfun (parse f, Var s1) 
    f'x = mkfun (parse f', Var s1)

-- Example Equations:
-- https://www.wolframalpha.com/input?i=abs%28%28+x*x*x%2Bx-1%29+%2F+%28%28%28%28%28x%2Bx%29*x%29%2B%28x*x%29%29%2B1%29%29-x%29%2C+x+%3D+1.0
-- https://www.wolframalpha.com/input?i=abs%28%28cos%28x%29*sin%28x%29%29+%2F+%28%28%28%28-1*sin%28x%29%29*sin%28x%29%29%2B%28cos%28x%29*cos%28x%29%29%29%29-x%29%2C+x+%3D+2.0
findHelp :: (Ord t, Fractional t) => (t -> t) -> (t -> t) -> t -> t
findHelp f f' xn
  | abs(prod - xn) <= 0.0001  = prod
  | otherwise                 = findHelp f f' prod
    where
      fx   = f  xn
      f'x  = f' xn  
      prod = abs $ xn - (fx/f'x)  


main :: IO()
main = do

  --Part 1
  print(parse "10")
  print(parse "x")
  print(parse "10+x")
  print(parse "1+2*(3-4/5)")
  
  --Part 2
  print(unparse (simplify (diff (Var "x") (parse "exp(sin(2*x))")))) --derivata

  --Part 3
  print(mkfun (parse "x*x+2", Var "x") 3.0) -- f(x) = 11

  -- Part 4
  print(findZero "x" "x*x*x+x-1" 1.0)       -- 0.68232775.
  print(findZero "y" "cos(y)*sin(y)" 2.0)   -- 1.5707964


{- 
-- Alt
findZero2 :: String -> String -> Float -> Float
findZero2 s1 s2 =  findHelp2 s1 f f' 
  where
    f = s2
    f' = unparse $ simplify (diff (Var s1) (parse s2))

findHelp2 :: String -> String -> String -> Float -> Float
findHelp2 s1 f f' xn
  | abs(pn1 - pn) <= 0.0001  = pn1
  | otherwise                = findHelp2 s1 f f' pn1 
    where
      str = "(((" ++ f ++ ")/(" ++ f' ++ "))-" ++ s1 ++ ")" 
      pn  = abs $ mkfun (parse str, Var s1) xn
      pn1 = abs $ mkfun (parse str, Var s1) pn

  print(findZero2 "x" "x*x*x+x-1" 1.0)       -- 0.68232775.
  print(findZero2 "y" "cos(y)*sin(y)" 2.0)   -- 1.5707964 -}