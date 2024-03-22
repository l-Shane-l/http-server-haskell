{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import Network.Socket.ByteString
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

checkMsg :: BC.ByteString -> BC.ByteString
checkMsg msg
  | path == "/" = "HTTP/1.1 200 OK\r\n\r\n"
  | "echo" `elem` pathComponents = content $ BC.intercalate "/" $ drop 2 pathComponents
  | otherwise = "HTTP/1.1 404 Not Found\r\n\r\n"
  where
    requestLines = BC.lines msg
    requestLine = head requestLines
    path = head $ drop 1 $ BC.words requestLine -- Extract the path part of the request line
    pathComponents = BC.split '/' path
    content echoText =
      BC.concat
        [ "HTTP/1.1 200 OK\r\n",
          "Content-Type: text/plain\r\n",
          "Content-Length: ",
          BC.pack . show $ BC.length echoText,
          "\r\n\r\n",
          echoText
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
    BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
    msg <- recv clientSocket 4000
    BC.putStrLn msg
    let resp = checkMsg msg
    let finalResp = resp
    BC.putStrLn finalResp
    sendAll clientSocket resp
    close clientSocket
