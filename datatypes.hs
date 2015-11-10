module DATATYPES where

import Data.ByteString.Char8 as BC
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
	} deriving Show

data Peer = Peer
	{ handle	  :: Handle
	, peerId 	  :: Maybe BC.ByteString
	, iInterested :: TVar Bool
	, pInterested :: TVar Bool
	, iChoking 	  :: TVar Bool
	, pChocking	  :: TVar Bool
	, bitField	  :: TVar BitField
	, waiting     :: TVar Bool
	}