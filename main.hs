import BENCODE

import qualified Data.Map as Map
import Data.Map(Map)
import Text.ParserCombinators.Parsec

import System.IO
import System.IO (hFlush, stdout)

readBencodeDict :: Bencode -> String -> Maybe Bencode
readBencodeDict (BEDictionary dict) key = Map.lookup key dict

main = do
    putStr "\nEnter filename: " >> hFlush stdout
    filename <- getLine
    
    inpHandle <- openFile filename ReadMode
    contents <- hGetContents inpHandle
    
    -- Convert torrent file to bencode
    let bencodeDict = maybe2Bencode1 $ parseBencoded contents
    --print bencodeDict

    let url = readBencodeDict bencodeDict "announce"
    let BEString announceURL = maybe2Bencode url


    print announceURL
