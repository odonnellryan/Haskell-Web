--{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
 -- Echo server program
module Main where
import Control.Monad (unless)
import Network.Socket hiding (recv)
import qualified Data.ByteString as S
import Network.Socket.ByteString

main :: IO ()
main = withSocketsDo $ do 
	addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "8080")
	let serveraddr = head addrinfos
	sock <- socket (addrFamily serveraddr) Stream defaultProtocol
	bindSocket sock (addrAddress serveraddr)
	listen sock 1
	loop sock
	sClose sock

loop :: Socket -> IO ()
loop sock = do
	(conn, _) <- accept sock
	talk conn
	sClose conn
	loop sock

talk :: Socket -> IO ()
talk conn = do 
	msg <- recv conn 1024
	print msg
	unless (S.null msg) $ do
		m <- build msg conn
		print m
		sendAll conn m
	--m <- testing msg conn

--build :: IO S.ByteString -> Socket -> IO S.ByteString
build :: S.ByteString -> Socket -> IO S.ByteString
build msg conn = do
    m <- recv conn 1024
    print m
    if S.null m then return msg
    else build (S.append msg m) conn

--serveFile :: String -> String
--serveFile line = line
--
--statusLine :: Int -> String -> Int -> [String]
--statusLine code status msgLength = ["HTTP/1.0", show code, status, "\r\nContent-Length:", show msgLength, "\r\n\r\n"]
--
--addStatus :: ReturnCode -> String -> String
--addStatus (ReturnCode code status) body = unwords (statusLine code status bLen) ++ body ++ "\r\n"
--	where
--		bLen = length body