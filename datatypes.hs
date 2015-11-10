module DATATYPES where

import Data.ByteString as BS
import Data.ByteString.Lazy as B
import Data.ByteString.Char8 as BC
import Data.Word
import Network
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import System.IO

type BitField = [Bool]

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
	} deriving (Show)

data Peer = Peer
	{ phandle	  :: Handle
	, peerId 	  :: Maybe BC.ByteString
	, iInterested :: TVar Bool
	, pInterested :: TVar Bool
	, iChoking 	  :: TVar Bool
	, pChocking	  :: TVar Bool
	, bitField	  :: TVar BitField
	, waiting     :: TVar Bool
	}

data PWP = Keepalive 
		 | Choke 
		 | Unchoke 
		 | Interested 
		 | Uninterested 
		 | Have Word32 
		 | BitField BitField 
		 | Request Word32 Word32 Word32
		 | Piece Word32 Word32 BC.ByteString
		 | Cancel Word32 Word32 Word32	deriving (Show)