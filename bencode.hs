{-# LANGUAGE RankNTypes #-} 				-- Rank2Types is a synonym for RankNTypes
module BENCODE where

import DATATYPES
import qualified Data.Map as Map
import Data.Map(Map)
import Text.ParserCombinators.Parsec
import Data.BEncode
import Data.ByteString.Lazy as B
import Data.ByteString.Char8 as BC
import Lens.Family2
import Data.Attoparsec.ByteString
import Control.Monad
import Control.Applicative
import Data.Traversable

bstring :: Traversal' BEncode B.ByteString
bstring f (BString s) = BString <$> f s
bstring _ bv = pure bv

bnumber :: Traversal' BEncode Integer
bnumber f (BInt n) = BInt <$> f n
bnumber _ bv = pure bv

blist :: Traversal' BEncode BEncode
blist f (BList xs) = BList <$> traverse f xs
blist _ bv = pure bv

bkey :: String -> Traversal' BEncode BEncode
bkey k f bv@(BDict m) = case Map.lookup k m of
                               Just v -> f v
                               Nothing -> pure bv
bkey _ _ bv = pure bv

--deparse :: BEncode -> B.ByteString
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