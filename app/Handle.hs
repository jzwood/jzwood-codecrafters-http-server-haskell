{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Handle (handle) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (pack)
import Data.Function
import Data.Functor
import System.Directory (doesFileExist)

--import Data.Attoparsec.ByteString.Char8 (Parser, char8, count, decimal, digit, endOfLine, isSpace, parseOnly, skipSpace, space, string, take, takeTill)
import Data.Attoparsec.ByteString.Char8 (Parser, endOfInput, parseOnly, string, takeByteString)
import qualified Data.ByteString as B
import Format
import Parser
import Syntax

toHeader :: ByteString -> ByteString -> (ByteString, ByteString)
toHeader = (,)

ok :: ByteString -> ByteString -> Resp
ok contentType body =
    Resp
        { protocol' = HTTP1_1
        , status = Status 200
        , headers' =
            [ toHeader "Content-Type" contentType
            , toHeader "Content-Length" ((pack . show . B.length) body)
            ]
        , body
        }

txt :: ByteString -> Resp
txt = ok "text/plain"

file :: ByteString -> Resp
file = ok "application/octet-stream"

notFound :: Resp
notFound =
    Resp
        { protocol' = HTTP1_1
        , status = Status 404
        , headers' = []
        , body = ""
        }

parseRoute :: Parser [ByteString]
parseRoute =
    (string "/" *> endOfInput $> ["/"])
        <|> (string "/user-agent" *> endOfInput $> ["user-agent"])
        <|> (string "/files/" *> takeByteString <&> \path -> ["files", path])
        <|> (string "/echo/" *> takeByteString <&> \echo -> ["echo", echo])

routeToResp :: Env -> Map -> [ByteString] -> IO Resp
routeToResp _ _ ["/"] = pure $ txt ""
routeToResp _ _ ["echo", echo] = pure $ txt echo
routeToResp _ headers ["user-agent"] = pure $ txt (getHeader "User-Agent" headers)
routeToResp Env { dir } _ ["files", bsPath] =
    B.toFilePath (dir <> "/" <> bsPath)
        >>= doesFileExist
        >>= \exists ->
            if exists
                then B.toFilePath (dir <> "/" <> bsPath) >>= B.readFile <&> file
                else pure notFound
routeToResp _ _ _ = pure notFound

handle' :: Env -> Req -> IO Resp
handle' env Req{path = (Path path), headers} =
    case parseOnly parseRoute path of
        Right bs -> routeToResp env headers bs
        Left _ -> pure notFound

handle :: Env -> ByteString -> IO ByteString
handle env bsReq =
    case runParser bsReq of
        Right req -> toBs <$> handle' env req
        Left _ -> pure $ toBs notFound
