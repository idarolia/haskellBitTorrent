module HTTP_REQ where

import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as C
import System.Random
import Control.Monad
import Network.Socket

genPeerID :: IO C.ByteString
genPeerID = do
			  let a = 1000000000000 :: Integer
			  randNum13 <- getStdRandom (randomR(a,9999999999999))
			  let peerId = "-HB0001" ++ show randNum13
			  return $ C.pack peerId
			  
makeTCPSock :: IO Socket