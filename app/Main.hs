{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkIO)
import Control.Monad ()
import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import Network.Socket.ByteString
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (BufferMode (..), hSetBuffering, stdin, stdout)

type Config = String -- Directory path

type App = ReaderT Config IO

checkMsg :: BC.ByteString -> App BC.ByteString
checkMsg msg = do
  if
    | path == "/" -> return ok
    | operation == "echo" -> return $ content $ BC.intercalate "/" $ drop 2 pathComponents
    | operation == "user-agent" -> return $ content userAgent
    | operation == "files" ->
        case (method, parseFileName path) of
          ("GET", Just filename) -> do
            liftIO $ putStrLn $ "Serving filename: " ++ BC.unpack filename -- For debugging
            result <- readFileIfExists filename
            case result of
              Right contents -> return $ buildResponse "200 OK" "application/octet-stream" contents
              Left _ -> return notFound
          ("POST", Just filename) -> do
            liftIO $ putStrLn $ "Saving filename: " ++ BC.unpack filename -- For debugging
            result <- saveFile filename (last requestLines)
            case result of
              Right _ -> return $ buildResponse "201 Created" "" "Created"
              Left errMsg -> return $ buildResponse "500 Internal Server Error" "" (BC.pack errMsg)
          _ -> return notFound
    | otherwise -> return notFound
  where
    requestLines = BC.lines msg
    requestLine = head requestLines
    path = BC.words requestLine !! 1
    pathComponents = BC.split '/' path
    operation = pathComponents !! 1
    content = buildResponse "200" "text/plain"
    userAgent = BC.words (requestLines !! 2) !! 1
    notFound = buildResponse "404 Not Found" "" "Not Found"
    ok = buildResponse "200 OK" "" "Welcome!"
    method = head $ BC.words requestLine

-- Extracts the filename from the request path, if it matches the expected format.
parseFileName :: BC.ByteString -> Maybe BC.ByteString
parseFileName path =
  case BC.split '/' path of
    ["", "files", filename] -> Just filename
    _ -> Nothing

saveFile :: BC.ByteString -> BC.ByteString -> App (Either String ())
saveFile filename body = do
  directory <- ask
  let filePath = directory ++ "/" ++ BC.unpack filename
  liftIO $ do
    BC.writeFile filePath body -- Write the file content
    return $ Right ()

readFileIfExists :: BC.ByteString -> App (Either String BC.ByteString)
readFileIfExists filename = do
  directory <- ask
  let filePath = directory ++ "/" ++ BC.unpack filename
  liftIO $ putStrLn $ "Attempting to access file: " ++ filePath
  fileExists <- liftIO $ doesFileExist filePath
  liftIO $ putStrLn $ "File exists: " ++ show fileExists
  if fileExists
    then do
      fileContents <- liftIO $ BS.readFile filePath
      return $ Right fileContents
    else return $ Left "File not found."

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

handleConnection :: Socket -> SockAddr -> App ()
handleConnection clientSocket clientAddr = do
  liftIO $ BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
  msg <- liftIO $ recv clientSocket 4000
  liftIO $ BC.putStrLn msg
  resp <- checkMsg msg
  liftIO $ BC.putStrLn "final resp \n"
  liftIO $ BC.putStrLn resp
  liftIO $ sendAll clientSocket resp
  liftIO $ close clientSocket

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stdin LineBuffering
  args <- getArgs
  let directory = args !! 1
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
    forkIO $ runReaderT (handleConnection clientSocket clientAddr) directory
