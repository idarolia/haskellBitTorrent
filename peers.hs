module PEERS where

import BENCODE
import DATATYPES


import Data.ByteString as BS
import Data.ByteString.Lazy as B
import Data.ByteString.Char8 as BC
import Network.Socket
import Network
import System.IO

connectPeers::[PeerAddress] -> IO Handle
connectPeers list = connectPeer (list !! 0)

connectPeer:: PeerAddress -> IO Handle
connectPeer (Address host (PortNumber port)) = do
									sock <- socket AF_INET Stream defaultProtocol
									sock1 <- getAddrInfo Nothing (Just host) (Just $ show port)
									connect sock (addrAddress $ Prelude.head sock1)
									handle <- socketToHandle sock ReadWriteMode
									input <- B.hGetContents handle
									return handle

sendHandshake:: Handle -> BC.ByteString -> BC.ByteString -> IO () -- not tested
sendHandshake handle infoHash peerId = BC.hPutStr handle handshake
									   where handshake = BS.concat[BS.singleton(fromIntegral 19), BC.pack "BitTorrent protocol", BS.replicate 8 (fromIntegral 0), infoHash, peerId ]
 							
receiveHandshake:: Handle -> IO (BC.ByteString,BC.ByteString,BC.ByteString,BC.ByteString,BC.ByteString) -- not tested
receiveHandshake handle =    do pstrlen <- BS.hGet handle 1
                                print $ pstrlen
                                pstr <- BS.hGet handle $ fromIntegral $ Prelude.head $ BS.unpack pstrlen
                                reserved <- BS.hGet handle 8
                                infoHash <- BS.hGet handle 20
                                peerId <- BS.hGet handle 20
                                return (pstrlen,pstr,reserved,infoHash,peerId)

--validateHandshake:: IO (BC.ByteString,BC.ByteString,BC.ByteString,BC.ByteString,BC.ByteString) -> infoHash -> Either String () 
validateHandshake (_,_,_,info_hash,_) infoHash

											|info_hash == infoHash = Right ()
											|otherwise = Left ("Peer InfoHash mismatch")
