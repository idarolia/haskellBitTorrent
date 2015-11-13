module Main where

import Torrent
import Bencode
import Httprequest
import Peers
import Datatypes

import qualified Data.Map as Map
import Data.Map(Map)
import Text.ParserCombinators.Parsec
import Data.BEncode
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as C
import Data.Word
import Data.ByteString.Char8 as BC
import Crypto.Hash.SHA1 as SHA1
import Data.Byteable
import Network.Socket
import System.IO as SIO
import System.Random
import System.IO (hFlush, stdout)
import Data.Binary.Put

main = do
    -- Path to the torrent file
    SIO.putStrLn "Input file: "
    filename <- SIO.getLine

    -- Path of the output file
    SIO.putStrLn "Output file: "
    output <- SIO.getLine
    
    -- Generating my Id for other Peers
    myPeerId <- genPeerID

    -- making a global Data Type "Torrent"
    tor <- makeTorrent filename output myPeerId

    peerList <- getPeerList tor
    print peerList

    -- Finally connecting to the peers for downloading
    connectPeers peerList tor

genPeerID :: IO BC.ByteString
genPeerID = do
              let a = 1000000000000 :: Integer
              randNum13 <- getStdRandom (randomR(a,9999999999999))
              let peerId = "-HB0001" ++ show randNum13
              return $ BC.pack peerId

getPeerList::Torrent -> IO [PeerAddress]
getPeerList tor = do
                    tcpSock <- makeTCPSock
                    temp <- socketPort tcpSock
                    let port = BC.pack $ show $ temp
                    peerList <- queryTracker tor port
                    return peerList