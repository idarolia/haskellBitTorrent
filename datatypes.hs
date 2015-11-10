module DATATYPES where

import Data.ByteString.Char8 as BC
import Network

data PeerAddress = Address {host :: HostName, port :: PortID} deriving (Show)

data Torrent = Torrent
	{ torrentName	:: String
	, announceURL	:: BC.ByteString             
	, infoHash		:: BC.ByteString        
	, myPeerId		:: BC.ByteString
	, piecesHash	:: [BC.ByteString]
	, pieceLength	:: Int
	, numPieces		:: Int
	, left			:: BC.ByteString
	} deriving Show