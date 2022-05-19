-- Victor Longberg Haskell lab 3 viclon-8@student.ltu.se

module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    If Expr.T Statement Statement   |
    Assignment String Expr.T        |
    Repeat Statement Expr.T         | 
    While Expr.T Statement          |
    Begin [Statement]               | 
    Write Expr.T                    |
    Read String                     |
    Skip                            
    deriving Show

{-
word -> String
Statement -> parse
Exper.Parse -> Exper.T
-}

{- Page 3 pdf.
program ::= statements
   statement ::= variable ':=' expr ';'
           | 'skip' ';'
           | 'begin' statements 'end'
           | 'if' expr 'then' statement 'else' statement
           | 'while' expr 'do' statement
           | 'read' variable ';'
           | 'write' expr ';'
   statements ::= {statement}
   variable ::= letter {letter}

   | ’repeat’ statement ’until’ expr ’;’ 
-}

--if 
if2 :: Parser Statement
if2 = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse  >-> buildIf2

buildIf2 :: ((Expr.T, Statement), Statement) -> Statement
buildIf2 ((es, s1) ,s2) = If es s1 s2

--Assignment
assignment :: Parser Statement
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss

buildAss :: (String, Expr.T) -> Statement
buildAss (str, es) = Assignment str es

--Repeat
repeater :: Parser Statement 
repeater  = accept "repeat" -# parse #- require "until" # Expr.parse #- require ";" >-> buildRepeat

buildRepeat :: (Statement, Expr.T) -> Statement
buildRepeat (s,es) = Repeat s es

--While
while :: Parser Statement
while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile

buildWhile :: (Expr.T, Statement) -> Statement
buildWhile (es,s) = While es s

--Begins
begin :: Parser Statement
begin = accept "begin" -# iter parse #- require "end" >-> buildBegin

buildBegin :: [Statement] -> Statement
buildBegin = Begin

--Write
write :: Parser Statement
write = accept "write" -# Expr.parse #- require ";" >-> buildWrite

buildWrite :: Expr.T -> Statement
buildWrite = Write

--Read
reader :: Parser Statement
reader = accept "read" -# word #- require ";" >-> buildReader -- #- word #- returns read

buildReader :: String -> Statement
--buildReader str = Read str
buildReader = Read

--Skip
skip :: Parser Statement
skip = accept "skip" # require ";" >-> buildSkip

buildSkip :: p -> Statement
buildSkip _ = Skip

--Exec
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec [] _ _ = []
exec (If es s1 s2: stmts) dict input = 
    if Expr.value es dict > 0 
    then exec (s1: stmts) dict input
    else exec (s2: stmts) dict input
--Just skips
exec (Skip : stmts) dict input = exec stmts dict input
--Inserts strin and int.   
exec (Read str : stmts) dict input = exec stmts (Dictionary.insert (str,head input) dict ) (tail input) 
--(exec stmts dict input) är av typen [int], Write är våran output skrivare, Jag vill "spara" mina statements i en array för min utskrift?
exec (Write es : stmts) dict input = Expr.value es dict : exec stmts dict input 
--Lägger till nya stments
exec (Begin as : stmts) dict input = exec ( as ++ stmts) dict input  -- vrf äre inte head as? Eftersom stmts är [T] och head as hade varit T (Statment)
--When s (do statement) holds run while
exec (While e s : stmts) dict input =
    if Expr.value e dict > 0
    then exec (s: While e s : stmts) dict input 
    else exec stmts dict input
-- When repeat s (repeat statement) holds run repeat else dont
exec (Repeat s e : stmts) dict input = 
    if Expr.value e dict <= 0
    then exec (s : Repeat s e : stmts) dict input
    else exec stmts dict input
 -- Adds operations into the dictionary?.
exec (Assignment str e : stmts) dict input = exec stmts (Dictionary.insert (str, Expr.value e dict) dict) input

instance Parse Statement where
  parse = assignment ! skip ! reader ! write ! begin ! while ! repeater ! if2
  toString (Assignment str es) = str ++ ":=" ++ Expr.toString es ++ ";\n"
  toString Skip = "skip;\n"
  toString (Read a) = "read " ++  a ++ ";\n"
  toString (Write a) = "write " ++ toString a ++ ";\n"
  toString (Begin as) = "begin\n" ++ concatMap toString as ++ "end"  -- linter magic
  toString (While es s) = "while " ++ Expr.toString es ++ " do\n" ++ toString s
  toString (Repeat s es) = "repeat\n" ++ toString s ++ "until " ++ Expr.toString es ++ "\n"
  toString (If es s1 s2) = "if" ++ Expr.toString es ++ " then \n" ++ toString s1 ++ "else\n" ++ toString s2

