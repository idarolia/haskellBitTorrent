module HTTP_REQ where

import BENCODE
import TORRENT
import DATATYPES
import Network.HTTP.Client
import Data.ByteString as BS
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Char8 as BC
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
import Data.Binary as Binary
import Data.Binary.Put

makeTCPSock :: IO Socket
makeTCPSock = do
			    sock <- socket AF_INET Stream defaultProtocol	--create Socket
			    bindSocket sock (SockAddrInet aNY_PORT iNADDR_ANY)
			    listen sock 5	-- number of connections allowed at a time
			    return sock

queryTracker:: Torrent -> BC.ByteString -> IO [PeerAddress]
queryTracker tor port = do
			url <- parseUrl $ BC.unpack (announceURL tor)
			let req = setQueryString [  (BC.pack "peer_id",Just $ myPeerId tor),
									(BC.pack "info_hash",Just $ infoHash tor),
									(BC.pack "compact",Just (BC.pack "1")),			--compact
									(BC.pack "port", Just port),			
									(BC.pack "uploaded",Just (BC.pack "0")),		--uploaded
									(BC.pack "downloaded",Just (BC.pack "0")),		--downloaded
									(BC.pack "left", Just $ left tor)	] url
			print req
			manager <- newManager defaultManagerSettings
			response <- httpLbs req manager
			let body = responseBody response
			case bRead body of 
				Just result -> return $ decodePeers $ result ^. (bkey "peers" . bstring)
				_ -> return []

decodePeer :: [Word8] -> PeerAddress	--Family2			--ipv6 ka kya karna hai?
decodePeer peer = let (ip,port) = L.splitAt 4 peer
                      host = L.intercalate "." $ Prelude.map show ip
                      (x:y:[]) = Prelude.map fromIntegral port
                    in Address host (PortNumber (y + x*256))

decodePeers:: C.ByteString -> [PeerAddress]
decodePeers peers = Prelude.map decodePeer $ chunksOf 6 $ BS.unpack $ C.toStrict peers								