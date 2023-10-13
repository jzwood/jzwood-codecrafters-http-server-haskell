{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (Parser, char8, count, decimal, digit, endOfLine, isSpace, parseOnly, skipSpace, space, string, take, takeTill)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromMaybe)
import Network.Simple.TCP (HostPreference (..), recv, send, serve)

import Data.Char (ord) -- DELETE THIS

ok :: ByteString
ok = "200 OK"

notFound :: ByteString
notFound = "404 Not Found"

get :: ByteString
get = "GET"

post :: ByteString
post = "POST"

packStr = B.pack . map (fromIntegral . ord) -- debug only

--emptyReq :: Req
--emptyReq =
--Req
--{ method = ""
--, path = ""
--, protocol = ""
--, headers = []
--, body = ""
--}

newtype Method = Method ByteString
    deriving (Eq, Show)

newtype Path = Path ByteString
    deriving (Eq, Show)

data Protocol = HTTP1_0 | HTTP1_1
    deriving (Eq, Show)

--newtype Protocol = Protocol ByteString
--deriving (Show)

data Req = Req
    { method :: Method
    , path :: Path
    , protocol :: Protocol
    --, headers :: [ByteString]
    --, body :: ByteString
    }
    deriving (Eq, Show)

class Res req where
    toRes :: req -> String

instance Res Req where
    toRes req = ""

parseMethod :: Parser Method
parseMethod = Method <$> (string "GET" <|> string "POST")

parsePath :: Parser Path
parsePath = Path <$> takeTill isSpace

parseProtocol :: Parser Protocol
parseProtocol = (string "HTTP/1.0" *> return HTTP1_0) <|> (string "HTTP/1.1" *> return HTTP1_1)

parseReq :: Parser Req
parseReq =
    Req <$> parseMethod
        <* space
        <*> parsePath
        <* space
        <*> parseProtocol

runParser :: ByteString -> Either String Req
runParser = parseOnly parseReq

main :: IO ()
main = do
    BLC.putStrLn "Logs from your program will appear here"

    let host = "127.0.0.1"
        port = "4221"

    BLC.putStrLn $ "Listening on " <> BLC.pack host <> ":" <> BLC.pack port
    serve (Host host) port $ \(serverSocket, serverAddr) -> do
        BLC.putStrLn $ "Accepted connection from " <> BLC.pack (show serverAddr) <> "."
        req <- recv serverSocket 1024
        let bs = fromMaybe B.empty req
        case runParser bs of
          Right Req { path = Path "/" } ->
            send serverSocket "HTTP/1.1 200 OK\r\n\r\n"
          Right req ->
            send serverSocket "HTTP/1.1 404 Not Found\r\n\r\n"
          Left _ ->
            send serverSocket "HTTP/1.1 404 Not Found\r\n\r\n"
