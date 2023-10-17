{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Handle (handle) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Functor
import Data.Function

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
        <|> (string "/echo/" *> takeByteString <&> \echo -> ["echo", echo])

routeToResp :: Map -> [ByteString] -> IO Resp
routeToResp _ ["/"] = pure $ txt ""
routeToResp _ ["echo", echo] = pure $ txt echo
routeToResp headers ["user-agent"] = pure $ txt (getHeader "User-Agent" headers)
routeToResp _ ["files", filename] = pure $ file "@TODO"
routeToResp _ _ = pure notFound

--handle :: Req -> Resp
--handle Req{path = (Path path), headers} =
    --case parseOnly parseRoute path of
        --Right bs -> routeToResp headers bs
        --Left _ -> notFound

handle' :: Req -> IO Resp
handle' = undefined

handle :: Env -> ByteString -> IO ByteString
handle env bsReq =
    case runParser bsReq of
      Right req -> toBs <$> handle' req
      Left _ -> pure $ toBs notFound
