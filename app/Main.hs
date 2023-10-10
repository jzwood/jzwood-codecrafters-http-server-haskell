{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (Parser, char8, count, decimal, endOfLine, parseOnly, string, take, takeTill)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromMaybe)
import Network.Simple.TCP (HostPreference (..), recv, send, serve)

ok :: ByteString
ok = "200 OK"

notFound :: ByteString
notFound = "404 Not Found"

get :: ByteString
get = "GET"

post :: ByteString
post = "POST"

--emptyReq :: Req
--emptyReq =
--Req
--{ method = ""
--, path = ""
--, protocol = ""
--, headers = []
--, body = ""
--}

data Method = Method ByteString
    deriving (Show)

data Req = Req
    { method :: ByteString
    --, path :: ByteString
    --, protocol :: ByteString
    --, headers :: [ByteString]
    --, body :: ByteString
    }
    deriving (Show)

parseMethod :: Parser Method
parseMethod = Method <$> (string "GET" <|> string "POST")

--runParser = parseOnly parse

--parseReq :: Parser Req
--parseSimpleString =
--Req <$> (string '+' *> takeTill isEOL <* endOfLine)

main :: IO ()
main = do
    BLC.putStrLn "Logs from your program will appear here"

    let host = "127.0.0.1"
        port = "4221"

    BLC.putStrLn $ "Listening on " <> BLC.pack host <> ":" <> BLC.pack port
    serve (Host host) port $ \(serverSocket, serverAddr) -> do
        BLC.putStrLn $ "Accepted connection from " <> BLC.pack (show serverAddr) <> "."
        res <- recv serverSocket 1024
        let bs = fromMaybe B.empty res
        B.putStr bs
        send serverSocket "HTTP/1.1 200 OK\r\n\r\n"
