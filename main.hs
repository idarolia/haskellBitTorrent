import BENCODE
import HTTP_REQ

import qualified Data.Map as Map
import Data.Map(Map)
import Text.ParserCombinators.Parsec
import Data.BEncode
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Char8 as BC
import Crypto.Hash.SHA1 as SHA1
import Crypto.Hash
import Network.Socket
import Data.Byteable
import System.IO
import System.IO (hFlush, stdout)
import Data.Binary.Put

main = do
    let filename = "debian-8.2.0-amd64-lxde-CD-1.iso.torrent"
    
    inpData <- B.readFile filename 
    let contents = bRead inpData

    let mainDict = maybeDict2Dict contents

    let url = readDict mainDict "announce"
    let announceURL = maybeString2String url

    let infoDict = readDict mainDict "info"
    let info = maybeDict2Dict infoDict
    
    let initLeft = BC.pack $ show $ findInitLeft info

    let pieceLen = readDict info "piece length"
    let pieceLength = maybeInt2Int pieceLen

    let piece = readDict info "pieces"
    let pieces = maybeString2String piece
    let numPieces = (B.length pieces) `div` 20

    let infoBencode = deparse (BDict info)
    let infoHash = toBytes $ SHA1.hashlazy (infoBencode)--BC.pack $ C.unpack $ B.fromStrict $ toBytes $ SHA1.hashlazy (infoBencode) -- hashlazy returns a bytestring instead of a digest bytestring

    peerId <- genPeerID
    tcpSock <- makeTCPSock
    temp <- socketPort tcpSock
    let port = BC.pack $ show $ temp
    let compact = BC.pack "1"
    let uploaded = BC.pack "0"
    let download = BC.pack "0"

    peerList <- queryTracker peerId infoHash compact port uploaded download initLeft announceURL
    --print $ announceURL
    --print $ infoHash
    --print contents
    print peerList
    handle <- connectPeers peerList
    sendHandshake handle infoHash peerId
    print "abc" 