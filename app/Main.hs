{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Data.List.Split
import Network.Socket
import Network.Socket.ByteString
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

checkMsg :: BC.ByteString -> String
checkMsg msg
  | path msg == "/" = "HTTP/1.1 200 OK\r\n\r\n"
  | url (path msg) !! 1 == "echo" = content (url (path msg) !! 2)
  | otherwise = "HTTP/1.1 404 Not Found\r\n\r\n"
  where
    msgParse msg''' = words $ BC.unpack msg'''
    firstLine parsedMsg = parsedMsg !! 1
    path msg' = firstLine $ msgParse msg'
    url = splitOn "/"
    content rand =
      "HTTP/1.1 200 OK\r\n"
        ++ "Content-Type: text/plain\r\n"
        ++ "Content-Length: "
        ++ show (length rand)
        ++ "\r\n\r\n"
        ++ rand

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
    let finalResp = BC.pack resp
    BC.putStrLn finalResp
    sendAll clientSocket (BC.pack resp)
    close clientSocket
