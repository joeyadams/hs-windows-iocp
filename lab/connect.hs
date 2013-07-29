{-# LANGUAGE OverloadedStrings #-}
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Network.Socket hiding (socket, connect)
import Network.Socket.Windows

import Control.Concurrent
import Control.Monad
import System.IO
import System.Timeout

server :: IO ()
server =
  void $ forkIO $ do
    sock <- socket AF_INET Stream defaultProtocol
    bind sock $ SockAddrInet 1234 iNADDR_ANY
    listen sock 5
    putStrLn "server: listening"
    forever $ do
        (client, clientAddr) <- accept sock
        putStrLn $ "server: accepted connection from " ++ show clientAddr
        forkIO $ do
            NSB.sendAll client "Hello!"
            bs <- NSB.recv client 4096
            putStrLn $ "server: received " ++ show bs
            threadDelay 1000000
            close client

main :: IO ()
main = withSocketsDo $ do
    mapM_ (`hSetBuffering` LineBuffering) [stdout, stderr]
    server

    google <- inet_addr "74.125.137.101"

    localhost <- inet_addr "127.0.0.1"
    sock <- socket AF_INET Stream defaultProtocol

    putStrLn $ "client: connecting to google.com:1234"
    timeout 2000000 (connect sock $ SockAddrInet 1234 google)
      >>= \m -> case m of
          Nothing -> do
              putStrLn "connect timed out.  Will try connecting to myself instead."
              connect sock $ SockAddrInet 1234 localhost
          Just () ->
              putStrLn "connect succeeded first time."

    putStrLn "client: connected to server"

    NSB.sendAll sock "Hello, server."

    bs <- NSB.recv sock 4096
    putStrLn $ "client: received " ++ show bs

    threadDelay 1000000
    close sock
