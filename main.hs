--{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Network
import System.IO

-- beginning of web server. just listens on that port and serves the test page.

main :: IO ()
main = do
	-- create the socket
	sock <- listenOn (PortNumber 8080)
	loop sock

loop :: Socket -> IO ()
loop sock = do
	-- accept the connection from the socket
	-- 
	(connection, hostName, portNumber) <- accept sock
	handleConn connection hostName portNumber
	loop sock

handleConn:: Handle -> HostName -> PortNumber -> IO ()
handleConn connection host port = do
	hPutStr connection (msg host port) 
	hFlush  connection
	hClose  connection

msg :: HostName -> PortNumber -> String
--msg = "HTTP/1.0 200 OK\r\nContent-Length: 5\r\n\r\n" ++ "\r\n"
msg host port = "HTTP/1.0 200 OK\r\nContent-Length: " ++ show (length(formatPortAndHost host port)) ++ "\r\n\r\n" ++ formatPortAndHost host port ++ "\r\n"

formatPortAndHost :: HostName -> PortNumber -> String
formatPortAndHost host port = "Host: " ++ show host ++ " Port: " ++ show port