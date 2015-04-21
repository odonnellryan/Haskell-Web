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

takeUntilCondition :: (a -> Bool) -> [IO a] -> IO [a]
takeUntilCondition predicate line = do
	x <- head line
	if predicate x
    	then takeUntilCondition predicate (tail line) >>= \xs -> return (x:xs)
    	else return []

handleConn :: Handle -> IO ()
handleConn handle = do
	request <- takeUntilCondition (/= "\r") (repeat (hGetLine handle))
	let method = head (words (head request))
	let line = unwords request
	case method of
		("GET") -> hPutStr handle (handleGet line)
		("POST") -> hPutStr handle (handlePost line)
		("PUT") -> hPutStr handle (handlePut line)
		("DELETE") -> hPutStr handle (handleDelete line)
		("HEAD") -> hPutStr handle (handleHead line)
		-- handle default case
		_ -> hPutStr handle (handleError "" (ReturnCode 443 "Invalid method."))
	hFlush handle
	hClose handle

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

serveFile :: String -> String
serveFile line = line

statusLine :: Int -> String -> Int -> [String]
statusLine code status msgLength = ["HTTP/1.0", show code, status, "\r\nContent-Length:", show msgLength, "\r\n\r\n"]

addStatus :: ReturnCode -> String -> String
addStatus (ReturnCode code status) body = unwords (statusLine code status bLen) ++ body ++ "\r\n"
	where
		bLen = length body
