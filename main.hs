import BENCODE

import qualified Data.Map as Map
import Data.Map(Map)
import Text.ParserCombinators.Parsec
import Data.BEncode
import Data.ByteString.Lazy as B
import Crypto.Hash.SHA1

import System.IO
import System.IO (hFlush, stdout)

main = do
    --filename <- getLine
    let filename = "newfile"
    
    inpData <- B.readFile filename 
    let contents = bRead inpData

    let mainDict = maybeDict2Dict contents

    let url = readDict mainDict "announce"
    let announceURL = maybeString2String url

    let infoDict = readDict mainDict "info"
    let info = maybeDict2Dict infoDict
    
    let initLeft = findInitLeft info

    let pieceLen = readDict info "piece length"
    let pieceLength = maybeInt2Int pieceLen

    let piece = readDict info "pieces"
    let pieces = maybeString2String piece
    let numPieces = (B.length pieces) `div` 20

    let infoBencode = deparse (BDict info)
    --print infoBencode
    let infoHash = B.fromStrict $ hashlazy (infoBencode)
    print infoHash
