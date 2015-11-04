module HTTP_REQ where

import BENCODE
import Network.HTTP.Client
import Data.ByteString as BS
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Char8 as BC
import System.Random
import System.IO
import Control.Applicative
import Control.Monad
import Network.Socket
import Data.BEncode
import Lens.Family2
import Network
import Data.List.Split
import Data.List as L
import Data.Word

data PeerAddress = Address {host :: HostName, port :: PortID} deriving (Show)

genPeerID :: IO BC.ByteString
genPeerID = do
			  let a = 1000000000000 :: Integer
			  randNum13 <- getStdRandom (randomR(a,9999999999999))
			  let peerId = "-HB0001" ++ show randNum13
			  return $ BC.pack peerId
			  
makeTCPSock :: IO Socket
makeTCPSock = do
			    sock <- socket AF_INET Stream defaultProtocol	--create Socket
			    bindSocket sock (SockAddrInet aNY_PORT iNADDR_ANY)
			    listen sock 5	-- number of connections allowed at a time
			    return sock

decodePeer :: [Word8] -> PeerAddress							--f
decodePeer peer = let (ip,port) = L.splitAt 4 peer
                      host = L.intercalate "." $ Prelude.map show ip
                      (x:y:[]) = Prelude.map fromIntegral port
                    in Address host (PortNumber (y + x*256))

decodePeers:: C.ByteString -> [PeerAddress]
decodePeers peers = Prelude.map decodePeer $ chunksOf 6 $ BS.unpack $ C.toStrict peers

queryTracker peerId infoHash compact port uploaded downloaded initLeft announceURL = do
			url <- parseUrl $ C.unpack announceURL
			let req = setQueryString [  (BC.pack "peer_id",Just peerId),
									(BC.pack "info_hash",Just infoHash),
									(BC.pack "compact",Just compact),
									(BC.pack "port", Just port),
									(BC.pack "uploaded",Just uploaded),
									(BC.pack "downloaded",Just downloaded),
									(BC.pack "left", Just initLeft)	] url
			print req
			manager <- newManager defaultManagerSettings
			response <- httpLbs req manager
			let body = responseBody response
			--print body
			case bRead body of 
				Just result -> return $ decodePeers $ result ^. (bkey "peers" . bstring)
				_ -> return []


connectPeer (Address host port) = do
									sock <- socket AF_INET Stream defaultProtocol
									connect sock (SockAddrInet port host)
									handle <- socketToHandle sock ReadWriteMode
									input <- B.hGetContents handle


--connectPeers::[PeerAddress]
connectPeers (x:xs) = connectPeer x

