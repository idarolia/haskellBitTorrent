module BENCODE where

import qualified Data.Map as Map
import Data.Map(Map)
import Text.ParserCombinators.Parsec
import Data.BEncode
import Data.ByteString.Lazy as B

deparse :: BEncode -> B.ByteString
deparse bencodeData = bPack bencodeData

readDict :: Map String BEncode -> String -> Maybe BEncode
readDict dict key = Map.lookup key dict

maybeInt2Int :: Maybe BEncode -> Integer
maybeInt2Int Nothing         = 0
maybeInt2Int (Just (BInt x)) = x

maybeString2String :: Maybe BEncode -> B.ByteString
maybeString2String Nothing            = B.empty
maybeString2String (Just (BString x)) = x

maybeList2List :: Maybe BEncode -> [BEncode]
maybeList2List Nothing          = []
maybeList2List (Just (BList x)) = x

maybeDict2Dict :: Maybe BEncode -> Map String BEncode
maybeDict2Dict Nothing          = Map.empty
maybeDict2Dict (Just (BDict m)) = m

getLenSum :: [BEncode] -> Integer
getLenSum []        = 0
getLenSum (x:xs)    = maybeInt2Int (Map.lookup "length" dict) + getLenSum xs
                      where BDict dict = x

findInitLeft :: Map String BEncode -> Integer
findInitLeft info   | Map.size info == 0        = 0
                    | fileList == Nothing   = maybeInt2Int len
                    | otherwise             = getLenSum $ maybeList2List fileList
                        where fileList  = Map.lookup "files" info
                              len       = Map.lookup "length" info
                              
