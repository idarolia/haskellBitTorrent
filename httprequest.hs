module HTTP_REQ where

--import Data.ByteString.From
--import Network.HTTP.Conduit
import Network.HTTP.Client
--import Network.HTTP.Client.Request
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Char8 as BC
import System.Random
import System.IO
import Control.Applicative
import Control.Monad
import Network.Socket
--import Network.HTTP
--import Network.URL

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
			    listen sock 5	-- number of connection allowed at a time
			    return sock

--queryTracker :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> 
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
			print response