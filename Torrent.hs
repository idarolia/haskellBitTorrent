module Torrent where

import Bencode
import Datatypes

import Data.ByteString.Lazy as B
import Data.ByteString.Char8 as BC
import Data.Word
import Crypto.Hash
import Crypto.Hash.SHA1 as SHA1
import Data.Byteable
import Data.BEncode
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import Data.List.Split
import qualified Data.Map as Map
import Data.Map(Map)


-- It initializes the global "Torrent" data type which is passed to each peer for further processing
-- It reads the given torrent file and decode its content using BEncode Library and bencode.hs
makeTorrent :: String -> String -> BC.ByteString -> IO Torrent
makeTorrent fname output mPeerId = do
					inpData <- B.readFile fname 
					let contents = bRead inpData
					let mainDict = maybeDict2Dict contents

					let url = readDict mainDict "announce"
					let aURL = B.toStrict $ maybeString2String url

					let infoDict = readDict mainDict "info"
					let info = maybeDict2Dict infoDict

					let initLeft = BC.pack $ show $ findInitLeft info

					let pieceLen = readDict info "piece length"
					let pLength = fromInteger $ maybeInt2Int pieceLen

					let piece = readDict info "pieces"
					let pieces = maybeString2String piece
					let pHash = Prelude.map BC.pack(chunksOf 20 (BC.unpack $ B.toStrict pieces))

					let nPieces = Prelude.length pHash

					let infoBencode = deparse (BDict info)
					let iHash = toBytes $ SHA1.hashlazy (infoBencode)
					c 		<- atomically (newTVar False)
					pp 		<- atomically (newTVar [])
					nextreq <- atomically (newTVar (Just(0,0)))
					pData 	<- atomically (newTVar $ Prelude.take nPieces $ Prelude.repeat [])

					return Torrent{
						torrentName 	= fname,
						outputFile		= output,
						announceURL 	= aURL,
						infoHash 		= iHash,
						myPeerId 		= mPeerId,
						piecesHash 		= pHash,
						pieceLength 	= pLength,
						numPieces 		= nPieces,
						left 			= initLeft,
						completed 		= c,
						presentPieces 	= pp,
						nextRequest 	= nextreq,
						piecesData		= pData
					}

findInitLeft :: Map String BEncode -> Integer
findInitLeft info   | Map.size info == 0        = 0
                    | fileList == Nothing   = maybeInt2Int len
                    | otherwise             = getLenSum $ maybeList2List fileList
                        where fileList  = Map.lookup "files" info
                              len       = Map.lookup "length" info

getLenSum :: [BEncode] -> Integer
getLenSum []        = 0
getLenSum (x:xs)    = maybeInt2Int (Map.lookup "length" dict) + getLenSum xs
                      where BDict dict = x