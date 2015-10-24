import BENCODE

import qualified Data.Map as Map
import Data.Map(Map)
import Text.ParserCombinators.Parsec
import Data.BEncode
import Data.ByteString.Lazy as B

import System.IO
import System.IO (hFlush, stdout)

main = do
    --filename <- getLine
    --let filename = "newfile"
    let filename = "file"
    
    inpData <- B.readFile filename 
    let contents = bRead inpData

    let mainDict = maybeDict2Dict contents

    let url = readDict mainDict "announce"
    let announceURL = maybeString2String url

    let infoDict = readDict mainDict "info"
    let info = maybeDict2Dict infoDict
    
    let initLeft = findInitLeft info
    print initLeft

