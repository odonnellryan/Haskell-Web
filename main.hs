--{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Network
import System.IO
import Data.List
--import Data.String.Utils

-- defining our custom types. We derive from Show so that 
-- we can use show on these types later!
data ReturnCode = ReturnCode Int String deriving (Show)
data ContentLength = ContentLength (Maybe Int) deriving (Show)
data Request = Request ContentLength deriving (Show)


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
	-- gets a list of strings
	-- searches the list for the first one that matches a pedicate
	-- in our case, this is the first blank line, ergo the end of the header
	x <- head line
	if predicate x
    	then do 
    		xs <- takeUntilCondition predicate (tail line) 
    		return (x:xs)
    	else return []


handleConn :: Handle -> IO ()
handleConn handle = do
	-- this gets from the handle until the predicate is met
	-- predicate here is the first blank like
	-- according to http spec this is the beginning of the body/end of header
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

--parseHeaders :: [String] -> Request
parseHeaders request = find (isInfixOf "Content-Length") request
	--getContentLength cs

setCL :: Request
setCL =
	Request (ContentLength (Just 110))

--getContentLength :: String -> ContentLength
getContentLength s = 
	let 
		repl "\\" = ""
		repl "r" = ""
		repl c = c
	in map repl t
	where
		w = words s
		t = tail w
		
		
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
