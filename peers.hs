module PEERS where

import BENCODE
import DATATYPES

import Control.Monad.STM
import Data.Monoid	((<>))
import Data.ByteString as BS
import Data.ByteString.Lazy as B
import Data.ByteString.Char8 as BC
import Data.Word
import Data.Bits
import Data.Bits.Bitwise
import Data.List as L
import Network.Socket
import Network
import Data.Binary (Binary)
import qualified Data.Binary as Bin
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar

makePeer::Handle -> STM Peer
makePeer h = do
				let peerid = Nothing
				iinterested <- newTVar False
				pinterested <- newTVar False
				ichoking <- newTVar True
				pchocking <- newTVar True
				bitfield <- newTVar []
				wait <- newTVar False
				return Peer{
					phandle = h,
					peerId = peerid,
					iInterested = iinterested,
					pInterested = pinterested,
					iChoking = ichoking,
					pChocking = pchocking,
					bitField = bitfield,
					waiting = wait
				}

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
					peer <- atomically . makePeer $ handle
					sendHandshake handle (infoHash tor) (myPeerId tor)
					res <- receiveHandshake handle
					case validateHandshake res (infoHash tor) of
						Nothing -> print "InfoHash Mismatch"
						Just () -> do
									listenPeer tor peer
									--race_ (listenPeer tor peer) (talkPeer tor peer)
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

validateHandshake:: (BC.ByteString,BC.ByteString,BC.ByteString,BC.ByteString,BC.ByteString) -> BC.ByteString -> Maybe ()
validateHandshake (_,_,_,info_hash,_) infoHash
											|info_hash == infoHash = Just ()
											|otherwise = Nothing

--listenPeer :: Torrent -> Peer -> IO ()
listenPeer tor peer = do
						let handle = phandle peer
						msg <- receiveMessage handle
						print msg
						case msg of
							Choke -> atomically (writeTVar (pChocking peer) True) >> print "choke"
							Unchoke -> atomically (writeTVar (pChocking peer) False) >> print "Unchoke"
							Interested -> atomically (writeTVar (pInterested peer) True) >> print "Interested"
							Uninterested -> atomically (writeTVar (pChocking peer) False) >> print "Uninterested"
							Have 
						return ()

receiveMessage :: Handle -> IO PWP
receiveMessage handle = do
						b <- B.hGet handle 4
						let len = fromIntegral (Bin.decode $ b :: Word32)
						m <- B.hGet handle len
						return $ Bin.decode (b <> m)

--bytestringToBool :: BC.ByteString -> [Bool]
--bytestringToBool body = L.foldr (++) [] (L.map ((\(a,b,c,d,e,f,g,h) -> [a,b,c,d,e,f,g,h]) . unpackWord8BE) (BS.unpack body))