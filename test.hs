import qualified Data.Map as Map
import Data.Map(Map)
import Text.ParserCombinators.Parsec

import System.IO
import System.IO (hFlush, stdout)
import Control.Monad
import Data.Char

data Bencode = BEInteger Integer 
             | BEString String 
             | BEList [Bencode] 
             | BEDictionary (Map String Bencode)
             deriving (Show, Eq)

number :: Parser Integer
number = 
    do n_str <- many1 digit 
       let n = read n_str
       return n

beString :: Parser Bencode
beString =
    do n <- number 
       char ':'
       str <- count (fromInteger n) anyChar
       return (BEString str) 


beInt :: Parser Bencode
beInt =
    do char 'i'
       n <- number
       char 'e'
       return (BEInteger n)

-- parse any Bencoded value
beParse :: Parser Bencode
beParse = beInt <|> beString <|> beDictionary <|> beList

beList :: Parser Bencode
beList =
    do char 'l'
       xs <- many beParse -- parse many bencoded values
       char 'e'
       return (BEList xs)

beDictionary1 :: Parser Bencode
beDictionary1 =
    do (BEString key) <- beString
       val <- beParse
       (BEDictionary m) <- beDictionary1
                           <|> beDictionary 
                           <|> do char 'e'
                                  return (BEDictionary Map.empty)

       return (BEDictionary (Map.insert key val m))


beDictionary :: Parser Bencode
beDictionary =
    do char 'd'
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

main = do
    putStr "\nEnter filename: " >> hFlush stdout
    filename <- getLine

    inpHandle <- openFile filename ReadMode
    contents <- hGetContents inpHandle
    print contents
    print $ parseBencoded contents