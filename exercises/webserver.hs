module Examples.PathServer where

import Examples.Phonebook
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types.Status (Status, status200, status404)
import Network.Wai (Application, Request, Response, responseLBS, pathInfo)
import Network.Wai.Handler.Warp (run)
import Database.SQLite.Simple

-- helper for constructing Responses
makeResponse :: Status -> T.Text -> Response
makeResponse status text =
  responseLBS status [] (B.fromStrict (encodeUtf8 text))

-- Serve the root path with "Hello World!"
serveRoot :: IO Response
serveRoot = return $ makeResponse status200 (T.pack "Hello World!")

-- Add a name and phone number to the phonebook
serveAdd :: Connection -> T.Text -> T.Text -> IO Response
serveAdd db name phone = do
  addToPhonebook db (T.unpack name) (T.unpack phone)
  return $ makeResponse status200 (T.pack "Successfully added!")

-- Query the phone numbers for a given name
serveQuery :: Connection -> T.Text -> IO Response
serveQuery db name = do
  numbers <- getNumbersFor db (T.unpack name)
  let responseText = T.pack $ show (length numbers) ++ " numbers \n" ++ unlines (map head numbers)
  return $ makeResponse status200 responseText

-- Handle different paths
servePath :: Connection -> [T.Text] -> IO Response
servePath db path
  | null path = serveRoot
  | [add, name, phone] <- path, add == T.pack "add" = serveAdd db name phone
  | [query, name] <- path, query == T.pack "query" = serveQuery db name
  | otherwise = serveNotFound path

serveNotFound :: [T.Text] -> IO Response
serveNotFound path =
  let showPath = T.intercalate (T.pack "/") path
      contents = T.append (T.pack "Not found: ") showPath
  in return $ makeResponse status404 contents

application :: Connection -> Application
application db request respond = do
  let path = pathInfo request
  response <- servePath db path
  respond response

port :: Int
port = 3421

main :: IO ()
main = do
  db <- openDatabase
  run port (application db)