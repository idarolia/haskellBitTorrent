module DATATYPES where

import Data.ByteString as BS
import Data.ByteString.Lazy as B
import Data.ByteString.Char8 as BC
import Data.Word
import Network
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Control.Applicative
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import System.IO

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
	, completed			:: TVar Bool
	, presentPieces :: TVar [Bool]
	} deriving (Show)

data Peer = Peer
	{ phandle	  :: Handle
	, peerId 	  :: Maybe BC.ByteString
	, iInterested :: TVar Bool
	, pInterested :: TVar Bool
	, iChoking 	  :: TVar Bool
	, pChocking	  :: TVar Bool
	, bitField	  :: TVar [Bool]
	, waiting     :: TVar Bool
	}

data PWP = Keepalive 
		 | Choke 
		 | Unchoke 
		 | Interested 
		 | Uninterested 
		 | Have Word32
		 | BitField BC.ByteString 
		 | Request Word32 Word32 Word32
		 | Piece Word32 Word32 BC.ByteString
		 | Cancel Word32 Word32 Word32 
		 deriving (Show)

instance Binary PWP where
	put Keepalive = put(0::Word32)
	put Choke = do
				put (1::Word32)
				put (0::Word8)
	put Unchoke = do
				put (1::Word32)
				put (1::Word8)
	put Interested = do
				put (1::Word32)
				put (2::Word8)
	put Uninterested = do
				put (1::Word32)
				put (3::Word8)
	put (Have pieceId) = do
				put (5::Word32)
				put (4::Word8)
				put pieceId
	put (BitField field) = do
				put (fromIntegral $ 1 + BC.length field :: Word32)
				put (5::Word8)
				putByteString field
	put (Request piece offset len) = do
				put (13::Word32)
				put (6::Word8)
				put piece
				put offset
				put len
	put (Piece piece offset d) = do
				put (fromIntegral $ 9 + BC.length d :: Word32)
				put (7::Word8)
				put piece
				put offset
				putByteString d
	put (Cancel piece offset len) = do
				put (13::Word32)
				put (8::Word8)
				put piece
				put offset
				put len

	get = do
		len <- get :: Get Word32
		case len of
			0 -> return Keepalive
			_ -> do
				msgId <- get :: Get Word8
				case msgId of
					0 -> return $ Choke
					1 -> return $ Unchoke
					2 -> return $ Interested
					3 -> return $ Uninterested
					4 -> Have <$> get
					5 -> BitField <$> getByteString (fromIntegral $ len-1)
					6 -> Request <$> get <*> get <*> get
					7 -> Piece <$> get <*> get <*> getByteString (fromIntegral $ len-9)
					8 -> Cancel <$> get <*> get <*> get
					_ -> fail "message not according to PWP protocol"
