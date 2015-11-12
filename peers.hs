module PEERS where

import BENCODE
import DATATYPES

import Control.Monad.STM
import Control.Monad (forever)
import Data.Monoid	((<>))
import Control.Applicative
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
import Control.Monad.Loops

makePeer::Handle -> STM Peer
makePeer h = do
				let peerid  = Nothing
				iinterested <- newTVar False
				pinterested <- newTVar False
				ichoking 	<- newTVar True
				pchocking 	<- newTVar True
				bitfield 	<- newTVar []
				wait 		<- newTVar False
				p 			<- newTVar False
				return Peer{
					phandle 	= h,
					peerId 		= peerid,
					iInterested = iinterested,
					pInterested = pinterested,
					iChoking 	= ichoking,
					pChocking 	= pchocking,
					bitField 	= bitfield,
					waiting 	= wait,
					pending 	= p
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

listenPeer :: Torrent -> Peer -> IO ()
listenPeer tor peer = forever $ do
						let handle = phandle peer
						msg <- receivePwpMessage handle
						print msg
						case msg of
							Choke -> atomically (writeTVar (pChocking peer) True) >> print "choke"
							Unchoke -> atomically (writeTVar (pChocking peer) False) >> print "Unchoke"
							Interested -> atomically (writeTVar (pInterested peer) True) >> print "Interested"
							Uninterested -> atomically (writeTVar (pInterested peer) False) >> print "Uninterested"
							Have n -> do
									bitfieldList <- readTVarIO (bitField peer)
									atomically (writeTVar (bitField peer) (newBitfield (word32ToInt n) bitfieldList)) 
									>> print ("Have" ++ (show (word32ToInt n)))
							BitField field -> do
											let boolField = bytestringToBool field
											atomically (writeTVar (bitField peer) boolField) 
											>> print "BitField"
							Piece pId os content -> do
											let pieceId = word32ToInt pId
											let offset = word32ToInt os
											print "Piece"
						return ()

talkWithPeer :: Torrent -> Peer -> IO ()
talkWithPeer tor peer = whileM_ (not <$> (atomically.readTVar) (completed tor)) (requestPeer tor peer)

receivePwpMessage :: Handle -> IO PWP
receivePwpMessage handle = do
						b <- B.hGet handle 4
						let len = fromIntegral (Bin.decode $ b :: Word32)
						m <- B.hGet handle len
						return $ Bin.decode (b <> m)

requestPeer :: Torrent -> Peer -> IO ()
requestPeer tor peer = do
						let handle = (phandle peer)
						pChoke <- (atomically.readTVar) (pChocking peer)
						case pChoke of
							True -> do
									let msg = Interested
									sendPwpMessage handle msg
							False -> do
									p <- (atomically.readTVar) (pending peer)
									case p of
										True -> threadDelay 100000
										False -> do
											req <- atomically (makeRequest tor)
											case req of
												Nothing -> return ()
												Just(pieceId,offset,len) -> do
														let msg = Request (intToWord32 pieceId) (intToWord32 offset) (intToWord32 len)
														sendPwpMessage handle msg
														atomically (writeTVar (pending peer) True)

sendPwpMessage :: Handle -> PWP -> IO ()
sendPwpMessage handle msg = do B.hPut handle (Bin.encode msg)

makeRequest::Torrent -> STM (Maybe (Int, Int, Int))
makeRequest tor = do
					nextreq <- readTVar (nextRequest tor)
					let pLen = (pieceLength tor)
					case nextreq of
						Nothing -> return Nothing
						Just(pId,offset) -> do
							if (pLen-offset) <= 16384 then do							-- blockSize = 16384
								let len = pLen-offset
								if (pId < (numPieces tor) -2) then do
									writeTVar (nextRequest tor) (Just((pId+1),0))		-- nextPiece to download
									return $ Just(pId,offset,len)
								else do
									writeTVar (nextRequest tor) Nothing
									writeTVar (completed tor) True					-- Download Complete after downloading this piece
									return $ Just(pId,offset,len)
							else do
								let len = 16384
								writeTVar (nextRequest tor) (Just(pId,offset+16384))
								return $ Just(pId,offset,len)

newBitfield :: Int -> [Bool] -> [Bool]
newBitfield n l = (L.take n l) ++ [True] ++ (L.drop (n+1) l)

word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral

intToWord32 :: Int -> Word32
intToWord32 = fromIntegral

bytestringToBool :: BC.ByteString -> [Bool]
bytestringToBool str = L.foldr (++) [] (L.map ((\(a,b,c,d,e,f,g,h) -> [a,b,c,d,e,f,g,h]) . unpackWord8BE) (BS.unpack str))