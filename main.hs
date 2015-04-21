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

-- takes io until the predicate is met, something something monad
takeUntilCondition :: (a -> Bool) -> [IO a] -> IO [a]
takeUntilCondition predicate line = do
	-- basically, checks if the line accepted against the predicate.
	-- continues if false
	x <- head line
	if predicate x
    	then takeUntilCondition predicate (tail line) >>= \xs -> return (x:xs)
    	else return []

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

--



serveFile :: String -> String
serveFile line = line

statusLine :: Int -> String -> Int -> [String]
statusLine code status msgLength = ["HTTP/1.0", show code, status, "\r\nContent-Length:", show msgLength, "\r\n\r\n"]

addStatus :: ReturnCode -> String -> String
addStatus (ReturnCode code status) body = unwords (statusLine code status bLen) ++ body ++ "\r\n"
	where
		bLen = length body
