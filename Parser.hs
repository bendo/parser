module Parser where

import           Control.Applicative
import           Data.Char           (isDigit)
import           Data.Functor        (($>))
import           Data.List           (isPrefixOf)

newtype Parser a = Parser { unParser :: String -> [(String, a)]}

parseString :: String -> Parser a -> Maybe a
parseString str (Parser p) = case p str of
    [("", val)] -> Just val
    _           -> Nothing

predP :: (Char -> Bool) -> Parser Char
predP predicate = Parser p
    where
        p (c:cs) = if (predicate c)
                    then [(cs,c)]
                    else []
        p "" = []

charP :: Char -> Parser Char
charP c = predP (== c)

stringP :: String -> Parser String
stringP s = Parser p
    where
        p input | input == s = [("", s)]
                | otherwise = []

skip :: (Char -> Bool) -> Parser ()
skip predicate = Parser p
    where
        p s = [(dropWhile predicate s,())]

prefixP :: String -> Parser String
prefixP str = Parser p
    where
        p input = if str `isPrefixOf` input
                  then [(drop (length str) input, str)]
                  else []

skipString :: String -> Parser ()
skipString str = prefixP str $> ()

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
instance Functor Parser where
    --fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser va) = Parser vb
        where
            --f :: a -> b
            --va :: String -> [(String, a)]
            --vb :: String -> [(String, b)]
            vb input = map (\(s, x) -> (s, f x)) (va input)

applyP :: Parser (b -> c) -> Parser b -> Parser c
applyP (Parser bc) (Parser b) = Parser c
    where
        c input = [(input'', f x) | (input', f) <- bc input,
                                    (input'', x) <- b input']

-- class Functor f => Applicative (f :: * -> *) where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
instance Applicative Parser where
    pure x = Parser (\s -> [(s, x)])
    pf <*> pa = applyP pf pa

data Struct = StructConstr Int Char String

pInt :: Parser Int
pInt = undefined

pChar :: Parser Char
pChar = undefined

pString :: Parser String
pString = undefined

parserStruct = StructConstr <$> pInt <*> pChar <*> pString

-- class Applicative f => Alternative f where
--    empty :: Parser a
--    (<|>) :: Parser a -> Parser a -> Parser a

instance Alternative Parser where
    empty = Parser (\_ -> [])
    (Parser p1) <|> (Parser p2) = Parser p3
        where
            p3 input = (p1 input) ++ (p2 input)

data Expr = ConstExpr Int | BinOpExpr Expr BinOp Expr | NegExpr Expr deriving Show
data BinOp = Plus | Mult deriving Show

parserExpr :: Parser Expr
parserExpr = contExprParser <|> binOpParser <|> negParser

contExprParser :: Parser Expr
contExprParser = ConstExpr <$> intParser

intParser :: Parser Int
intParser = Parser p
    where
        digitParser = predP isDigit
        p input = case unParser (some digitParser) input of
            []                -> []
            ((rest, str) : _) -> [(rest, read str)]

binOpParser :: Parser Expr
binOpParser = charP '(' *>
    (BinOpExpr <$>
      parserExpr <*>
      (charP ' ' *> opParser <* charP ' ') <*>
      parserExpr
    )
    <* charP ')'

opParser :: Parser BinOp
opParser = plusParser <|> mulParser
    where
        plusParser = charP '+' $> Plus
        mulParser = charP '*' $> Mult

negParser :: Parser Expr
negParser = charP '-' *> (NegExpr <$> parserExpr)
