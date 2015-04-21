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
	let l = repeat (recv conn 1024)
	-- feed until we get the first line that is a carriage return
	l2 <- takeUntilCondition (/= "\r") l
	print l2
	--m <- testing msg conn

--build :: IO S.ByteString -> Socket -> IO S.ByteString
takeUntilCondition :: (a -> Bool) -> [IO a] -> IO [a]
takeUntilCondition predicate line = do
	-- basically, checks if the line accepted against the predicate.
	-- continues if false
	x <- head line
	if predicate x
    	then takeUntilCondition predicate (tail line) >>= \xs -> return (x:xs)
    	else return []

<<<<<<< HEAD
--serveFile :: String -> String
--serveFile line = line
=======
handleConn :: Handle -> IO ()
handleConn handle = do
	-- we get the request from the client
	-- this probably should be using bytestrings but whatever
	-- update: definitely use bytestrings, otherwise we've crossed the hacky line
	let l = repeat (hGetLine handle)
	-- feed until we get the first line that is a carriage return
	l2 <- takeUntilCondition (/= "\r") l
	print l2
	-- split the request into a list of params. first is going to be the METHOD.
	let request = words (head l2)
	print (tail l2)
	let line = head l2
	case head request of
		-- not sure if this is best, but I guess we'll see!
		("GET") -> hPutStr handle (handleGet line)
		("POST") -> hPutStr handle (handlePost line)
		("PUT") -> hPutStr handle (handlePut line)
		("DELETE") -> hPutStr handle (handleDelete line)
		("HEAD") -> hPutStr handle (handleHead line)
		-- handle default case
		_ -> hPutStr handle (handleError "" (ReturnCode 443 "Invalid method."))
	hFlush handle
	hClose handle



-- begin handlers

handleGet :: String -> String
handleGet line = addStatus returnCode body
	where
		body = serveFile line
		returnCode = ReturnCode 200 "OK"

handlePost :: String -> String
handlePost line = "test: " ++ line

handlePut :: String -> String
handlePut line = "test: " ++ line

handleDelete :: String -> String
handleDelete line = "test: " ++ line

handleHead :: String -> String
handleHead line = "test: " ++ line

handleError :: String -> ReturnCode -> String
handleError body code = addStatus code body

>>>>>>> parent of 342dabc... Oops
--
--statusLine :: Int -> String -> Int -> [String]
--statusLine code status msgLength = ["HTTP/1.0", show code, status, "\r\nContent-Length:", show msgLength, "\r\n\r\n"]
--
--addStatus :: ReturnCode -> String -> String
--addStatus (ReturnCode code status) body = unwords (statusLine code status bLen) ++ body ++ "\r\n"
--	where
--		bLen = length body
