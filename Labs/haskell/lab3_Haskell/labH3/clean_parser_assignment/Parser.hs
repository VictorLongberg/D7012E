-- Victor Longberg Haskell lab 3 viclon-8@student.ltu.se
module Parser(module CoreParser, T, digit, digitVal, chars, letter, err, lit, number, iter, accept, require, token, spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

--Basically exercises.

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

-- Håkans iterate function, iterates m times.
iterate' :: Parser a -> Int -> Parser [a]
iterate' m 0 = return []
iterate' m i = m # iterate' m (i-1) >-> cons

cons(a, b) = a:b

--Exercise1
semicolon :: Parser Char
semicolon (';' : xs) = Just (';', xs)
semicolon _ = Nothing 

--Exercise2
becomes :: Parser Char
becomes ('=' : xs) = Just('=', xs)
becomes _ = Nothing

--Exercise3
char2 :: Parser Char
char2 [] = Nothing
char2 (cs : xs) =  Just(cs , xs)

--Exercise4
letter2, space2, digit2 :: Parser Char 
letter2 = (?) char2 isAlpha
space2 = (?) char2 isSpace
digit2 = (?) char2 isDigit

--Exercise5
alphanum :: Parser Char
alphanum = (!) letter2 digit2

--Exercise6
lit2 :: Char -> Parser Char
lit2 c = (?) char2 (== c)

semicolon2 :: Parser Char
semicolon2 = lit2 ';'

--Exercise7
becomes2 :: Parser (Char,Char)
becomes2 = (?) twochars (\(x,y) -> x=='=' && y=='=')

twochars :: Parser (Char, Char)
twochars = (#) char char

--Exercise8
digitVal2 :: Parser Int
digitVal2 = (>->) digit2 digitToInt

letterUpp :: Parser Char
letterUpp = (>->) letter toUpper

--Exercise9
sndchar :: Parser Char
sndchar = (>->) twochars snd

--Exercise10
twochars2 :: Parser String
--twochars2 xs = Just (take 2 xs, drop 2 xs)
twochars2 xs = Just (splitAt 2 xs)

--Exercise11
(-#) :: Parser a -> Parser b -> Parser b
(-#) m n = (#) m n >-> snd

--Exercise12
(#-) :: Parser a -> Parser b -> Parser a
(#-) m n = (#) m n >-> fst

spaces :: Parser String
spaces = iter $ (?) char isSpace

token :: Parser a -> Parser a
token m = m #- spaces

letter :: Parser Char
letter = (?) char isAlpha

word :: Parser String
word = token (letter # iter letter >-> cons)

chars :: Int -> Parser String
--chars n =  iterate' char n
chars =  iterate' char

accept :: String -> Parser String
accept w = token (chars (length w)) ? (==w)

require :: String -> Parser String
require w  = accept w ! err warning
    where
        warning = "Wrong " ++ w

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n

number :: Parser Integer
number = token (digitVal #> number')
