module Parser where

import           Data.List (isPrefixOf)

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
skipString str = Parser p
    where
        p input = if str `isPrefixOf` input
                  then [(drop (length str) input, ())]
                  else []
