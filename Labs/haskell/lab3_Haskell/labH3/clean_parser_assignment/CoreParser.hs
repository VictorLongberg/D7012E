-- Victor Longberg Haskell lab 3 viclon-8@student.ltu.se
module CoreParser(Parser, char, return, fail, (#), (!), (?), (#>), (>->),Parse, parse, toString, fromString) where
import Prelude hiding (return, fail, isA)
import Data.Char

infixl 3 ! 
infixl 7 ?
infixl 6 #
infixl 5 >->
infixl 4 #>

-- Inflix introduce parsers
-- The numbers determine the realtive higharchy of operations.

class Parse a where
    parse :: Parser a
    fromString :: String -> a
    fromString cs =
        case parse cs of
               Just(s, []) -> s
               Just(s, cs) -> error ("garbage '"++cs++"'")
               Nothing -> error "Nothing"
    toString :: a -> String

-- Just is a constructor, Maybe is a value.
-- Mabey(a,String) --> Just(a,String) | Nothing
type Parser a = String -> Maybe (a, String)

-- The basic three.
--returns the first char from a string.
char :: Parser Char
char []= Nothing
char (c:cs) = Just (c, cs)

--returns the parser object, Parser a = String -> Mabey(a,String) -> Just(a,String) | Nothing
return :: a -> Parser a
return a cs = Just (a, cs)

--returns Nothing. 
fail ::  Parser a 
fail cs = Nothing


--Alternative, like or (||) operator but for Parser a. When nothing try other parser.
(!) :: Parser a -> Parser a -> Parser a
(m ! n) cs = case m cs of
             Nothing -> n cs 
             mcs -> mcs

{- 
https://www.youtube.com/watch?v=xs0IJItyTUs&list=PL_aTLqb10zrJLwvwQeypNqqQJw7x94Us6&index=6

Test operator.
Two arugment function.
(a->Bool) is called a predicut function which takes type a and returns a bool.

(a->Bool) is a test, if the test ever fails we return nothing, if not we return Parser a

m is the parser
p is the predicut.

we run the parser on the sring cs.
if fails we return nothing.

Just result, string returns if we can parse the result then we return the Parser for

the result is the preditional conditon "-> if p result then Just(result, string) else Nothing"
Just(result,string) otherwhise nothing
 -}
 --Parse input and return check output relative function
(?) :: Parser a -> (a -> Bool) -> Parser a
(?) m p cstring = 
    case m cstring of
    Nothing -> Nothing
    Just(result, string) -> if p result then Just(result, string) else Nothing

-- Sequence, a pipe function to combine two Parsers to a Tuple Parser
(#) :: Parser a -> Parser b -> Parser (a, b)
(m # n) cs = 
    case m cs of
    Nothing -> Nothing
    Just(a, cs') -> 
        case n cs' of
        Nothing -> Nothing
        Just(b, cs'') -> Just((a, b), cs'')

-- Trasnformation, Transform, perform a operation on the parser
(>->) :: Parser a -> (a -> b) -> Parser b
(m >-> b) cs = 
    case m cs of
    Just(a, cs') -> Just(b a, cs')
    Nothing -> Nothing

--Transfer 
(#>) :: Parser a -> (a -> Parser b) -> Parser b 
(p #> k) cs = 
    case p cs of
    Nothing -> Nothing
    Just(a, cs') -> k a cs'
