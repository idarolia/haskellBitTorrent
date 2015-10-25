module HTTP_REQ where

import Data.ByteString.Lazy as B
import Data.ByteString.Char8 as C
import System.Random

genPeerID :: IO C.ByteString
genPeerID = do
			  randNum13 <- getStdRandom (randomR(1000000000000,9999999999999))
			  let peerId = "-HB0001" ++ show randNum13
			  return $ C.pack peerId
			  