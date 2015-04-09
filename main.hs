--{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Network
import System.IO

-- defining our custom types. We derive from Show so that 
-- we can use show on these types later!
data ReturnCode = ReturnCode Int String deriving (Show)


-- beginning of web server. just listens on that port and serves the test page.

main :: IO ()
main = do
	-- create the socket
	sock <- listenOn (PortNumber 8080)
	loop sock

loop :: Socket -> IO ()
loop sock = do
	-- accept the connection from the socket
	(connection, hostName, portNumber) <- accept sock
	handleConn connection hostName portNumber
	loop sock

handleConn :: Handle -> HostName -> PortNumber -> IO ()
handleConn connection host port = do
	hPutStr connection (msg host port) 
	hFlush  connection
	hClose  connection

msg :: HostName -> PortNumber -> String
msg host port = wrapWithHeaders returnCode body
	where 
		body = show (formatPortAndHost host port)
		returnCode = ReturnCode 200 "OK"
		
formatPortAndHost :: HostName -> PortNumber -> String
formatPortAndHost host port = "Host: " ++ show host ++ " Port: " ++ show port

-- not sure if this will work, having a moment.
httpHeaders :: Int -> String -> Int -> [String]
httpHeaders code status msgLength = ["HTTP/1.0", show code, status, "\r\nContent-Length:", show msgLength, "\r\n\r\n"]

wrapWithHeaders :: ReturnCode -> String -> String
wrapWithHeaders (ReturnCode code status) body = unwords (httpHeaders code status bLen) ++ body ++ "\r\n"
	where
		bLen = length body