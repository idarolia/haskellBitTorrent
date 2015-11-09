module TORRENT where

data Torrent = Torrent
	{ torrentName	:: String
	, announceURL	:: ByteString             
	, infoHash		:: ByteString        
	, myPeerId		:: ByteString
	, piecesHash	:: [ByteString]
	, pieceLength	:: Int
	, numPieces		:: Int
	, left			:: Int
	}

 
