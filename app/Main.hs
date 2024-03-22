{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Network.Socket
import Network.Socket.ByteString
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

checkMsg :: BC.ByteString -> BC.ByteString
checkMsg msg
  | path == "/" = buildResponse "200 OK" "text/plain" ""
  | path == "/user-agent" =
      let userAgent = fromMaybe "User-Agent not found" $ lookup "user-agent" (parseHeaders msg)
       in buildResponse "200 OK" "text/plain" userAgent
  | url (path) !! 1 == "echo" = content $ BC.intercalate "/" $ drop 2 $ url path
  | otherwise = "HTTP/1.1 404 Not Found\r\n\r\n"
  where
    requestLines = BC.lines msg
    requestLine = head requestLines
    path = head $ drop 1 $ BC.words requestLine -- Extract the path
    url = BC.split '/' -- A function to split paths into components
    -- Parses headers into a list of (key, value) tuples
    parseHeaders request =
      let headers = takeWhile (/= "") . drop 1 . dropWhile (/= "") $ requestLines
       in map
            ( \header ->
                let (key, value) = BC.break (== ':') header
                 in (BC.map toLower $ BC.strip key, BC.strip $ BC.drop 1 value)
            )
            headers
    -- Builds a generic HTTP response given status, content type, and body
    buildResponse status contentType body =
      BC.concat
        [ "HTTP/1.1 ",
          status,
          "\r\n",
          "Content-Type: ",
          contentType,
          "\r\n",
          "Content-Length: ",
          BC.pack . show $ BC.length body,
          "\r\n\r\n",
          body
        ]
    content echoText =
      buildResponse "200 OK" "text/plain" echoText

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
