module PEERS where

import BENCODE
import DATATYPES

import Control.Monad.STM
import Data.ByteString as BS
import Data.ByteString.Lazy as B
import Data.ByteString.Char8 as BC
import Network.Socket
import Network
import System.IO
import Control.Exception
import Control.Concurrent

connectPeers::[PeerAddress] -> Torrent -> IO ()
connectPeers peerList tor = do
							mapM_ (connectPeer tor) peerList
							--atomically $ do
							--	finished <- readTVar (done tor)
							--	if finished
							--		then return ()
							--		else retry
							print "Downloading Completed!\n"

startPeer:: Torrent -> Handle -> IO ()
startPeer tor handle = do
					print "Bunny"
					return ()

--connectPeer:: Torrent -> PeerAddress -> 
connectPeer tor peerAddr = let start = bracket (getPeerHandle peerAddr) (closeHandle peerAddr) (startPeer tor)
							in forkFinally start (handleException peerAddr)

closeHandle::PeerAddress -> Handle -> IO ()
closeHandle (Address host (PortNumber port)) handle = do
								print $ ("Connection Close: ") ++ show host ++ ":" ++ show port
								hClose handle

handleException:: PeerAddress -> Either SomeException a -> IO ()
handleException (Address host (PortNumber port)) (Right _) = print $ "Peer: " ++ show host ++ ":" ++ show port ++ " Done."
handleException (Address host (PortNumber port)) (Left e) = print $ "Exception in Peer: " ++ show host ++ ":" ++ show port ++ " error:" ++ show(e)

getPeerHandle:: PeerAddress -> IO Handle
getPeerHandle (Address host (PortNumber port)) = do
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
