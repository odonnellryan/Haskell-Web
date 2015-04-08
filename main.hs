{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Network.Socket

main :: IO ()
main = do
	sock <- socket AF_INET Stream 0
	setSocketOption sock ReuseAddr 1
	bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
	listen sock 2
	loop sock

loop sock = do
	connection <- accept sock
	handleConn connection
	loop sock

handleConn (sock, _) = do
	_ <- send sock "Testing!!\n"
	sClose sock