module BENCODE where

import qualified Data.Map as Map
import Data.Map(Map)
import Text.ParserCombinators.Parsec

data Bencode = BEInteger Integer 
             | BEString String 
             | BEList [Bencode] 
             | BEDictionary (Map String Bencode)
             | BENull
             deriving (Show, Eq)

number :: Parser Integer
number = 
    do  n_str <- many1 digit 
        let n = read n_str
        return n

beString :: Parser Bencode
beString =
    do  n <- number 
        char ':'
        str <- count (fromInteger n) anyChar
        return (BEString str) 


beInt :: Parser Bencode
beInt =
    do  char 'i'
        n <- number
        char 'e'
        return (BEInteger n)

beParse :: Parser Bencode
beParse = beInt <|> beString <|> beDictionary <|> beList

beList :: Parser Bencode
beList =
    do  char 'l'
        xs <- many beParse               -- parse many bencoded values
        char 'e'
        return (BEList xs)

beDictionary1 :: Parser Bencode
beDictionary1 =
    do  (BEString key) <- beString
        val <- beParse
        (BEDictionary m) <- beDictionary1
                           <|> beDictionary 
                           <|> do char 'e'
                                  return (BEDictionary Map.empty)
        return (BEDictionary (Map.insert key val m))


beDictionary :: Parser Bencode
beDictionary =
    do  char 'd'
        (BEString key) <- beString
        val <- beParse
        (BEDictionary m) <- beDictionary1 
                           <|> do char 'e'
                                  return (BEDictionary Map.empty)

        return (BEDictionary (Map.insert key val m))

-- main parser function
parseBencoded :: String -> Maybe [Bencode]
parseBencoded str = case parse (many beParse) "" str of
                                Left err -> Nothing
                                Right val -> Just val

-- convert parseBencoded to Bencode
maybe2Bencode1 :: Maybe[Bencode] -> Bencode
maybe2Bencode1 Nothing       = BENull
maybe2Bencode1 (Just (x:xs)) = x

maybe2Bencode :: Maybe Bencode -> Bencode
maybe2Bencode Nothing       = BENull
maybe2Bencode (Just x)      = x
