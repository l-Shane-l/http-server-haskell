{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import Network.Socket.ByteString
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

checkMsg :: BC.ByteString -> BC.ByteString
checkMsg msg
  | path == "/" = buildResponse "200 OK" "" "Welcome!"
  | operation == "echo" = content $ BC.intercalate "/" $ drop 2 pathComponents
  | operation == "user-agent" = content userAgent
  | otherwise = buildResponse "404 Not Found" "" "Not Found"
  where
    requestLines = BC.lines msg
    requestLine = head requestLines
    path = BC.words requestLine !! 1
    pathComponents = BC.split '/' path
    operation = pathComponents !! 1
    content = buildResponse "200" "text/plain"
    userAgent = BC.words (requestLines !! 2) !! 1

buildResponse :: BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString
buildResponse status contentType body =
  BC.concat
    [ "HTTP/1.1 ",
      status,
      "\r\n",
      if BC.null contentType then "" else BC.concat ["Content-Type: ", contentType, "\r\n"],
      "Content-Length: ",
      BC.pack . show $ BC.length body,
      "\r\n\r\n",
      body
    ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  -- You can use print statements as follows for debugging, they'll be visible when running tests.
  BC.putStrLn "Logs from your program will appear here"

  let host = "127.0.0.1"
      port = "4221"

  BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

  -- Get address information for the given host and port
  addrInfo <- getAddrInfo Nothing (Just host) (Just port)

  serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
  setSocketOption serverSocket ReuseAddr 1 -- This stops a busy socket error
  withFdSocket serverSocket setCloseOnExecIfNeeded
  bind serverSocket $ addrAddress $ head addrInfo
  listen serverSocket 5

  -- Accept connections and handle them forever
  forever $ do
    (clientSocket, clientAddr) <- accept serverSocket
    forkIO $ do
      BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
      msg <- recv clientSocket 4000
      BC.putStrLn msg
      let resp = checkMsg msg
      let finalResp = resp
      putStrLn "final resp \n"
      BC.putStrLn finalResp
      sendAll clientSocket resp
      close clientSocket
