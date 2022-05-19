-- Victor Longberg Haskell lab 3 viclon-8@student.ltu.se
module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T]
instance Parse T where
  parse = iter Statement.parse >-> Program 
  toString (Program s) =  mconcat $ map Statement.toString s --https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html

exec (Program s)  = Statement.exec s Dictionary.empty -- Den vill ha type dict och empty var den enda vettiga
















--Notes.
-- Statment exec: exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

{-

Filerna i TestProgram har 3 variablers med Program exec som tar två.

p -> fromString "read k;" .....
  fromString :: CoreParser.Parse a => String -> a
  Parser a klassen i CoreParser.

-- Test programet kallar på program exec med p1 [intArr]


rp = Program.exec p [3,16]

rp1 = Program.exec p1 [1024, 2]

rp2 = Program.exec p2 [10]

-}