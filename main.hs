--{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Network
import System.IO

-- defining our custom types. We derive from Show so that 
-- we can use show on these types later!
data ReturnCode = ReturnCode Int String deriving (Show)

-- beginning of web server. just listens on that port and serves a basic page

main :: IO ()
main = do
	-- create the socket
	sock <- listenOn (PortNumber 8080)
	loop sock

loop :: Socket -> IO ()
loop sock = do
	-- accept the handle from the socket
	(handle, _, _) <- accept sock
	handleConn handle
	loop sock

handleConn :: Handle -> IO ()
handleConn handle = do
	-- we get the request from the client
	line <- hGetLine handle
	-- split the request into a list of params. first is going to be the METHOD.
	-- this probably works because most(?) times the params sent fromt he client are
	-- space delimited. i think.
	let method = words line
	case head method of
		-- not sure if this is best, but I guess we'll see!
		("GET") -> hPutStr handle (handleGet line)
		("POST") -> hPutStr handle (handlePost line)
		("PUT") -> hPutStr handle (handlePut line)
		("DELETE") -> hPutStr handle (handleDelete line)
		("TRACE") -> hPutStr handle (handleTrace line)
		("CONNECT") -> hPutStr handle (handleConnect line)
		_ -> hPutStr handle (handleError line)
	hFlush  handle
	hClose  handle

handleGet :: String -> String
handleGet line = addHeader returnCode body
	where
		body = serveFile line
		returnCode = ReturnCode 200 "OK"

handlePost :: String -> String
handlePost line = "test: " ++ line

handlePut :: String -> String
handlePut line = "test: " ++ line

handleDelete :: String -> String
handleDelete line = "test: " ++ line

handleTrace :: String -> String
handleTrace line = "test: " ++ line

handleConnect :: String -> String
handleConnect line = "test: " ++ line

handleError :: String -> String
handleError line = addHeader returnCode body
	where
		body = serveFile line
		returnCode = ReturnCode 200 "OK"


serveFile :: String -> String
serveFile line = line ++ line

httpHeaders :: Int -> String -> Int -> [String]
httpHeaders code status msgLength = ["HTTP/1.0", show code, status, "\r\nContent-Length:", show msgLength, "\r\n\r\n"]

addHeader :: ReturnCode -> String -> String
addHeader (ReturnCode code status) body = unwords (httpHeaders code status bLen) ++ body ++ "\r\n"
	where
		bLen = length body