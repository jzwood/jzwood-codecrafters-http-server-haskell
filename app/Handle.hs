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

ok :: ByteString -> Resp
ok body =
    Resp
        { protocol' = HTTP1_1
        , status = Status 200
        , headers' =
            [ toHeader "Content-Type" "text/plain"
            , toHeader "Content-Length" ((pack . show . B.length) body)
            ]
        , body
        }

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

routeToResp :: Map -> [ByteString] -> Resp
routeToResp _ ["/"] = ok ""
routeToResp _ ["echo", echo] = ok echo
routeToResp headers ["user-agent"] = ok (getHeader "User-Agent" headers)
routeToResp _ ["files", filename] = ok "@TODO"
routeToResp _ _ = notFound

--handle :: Req -> Resp
--handle Req{path = (Path path), headers} =
    --case parseOnly parseRoute path of
        --Right bs -> routeToResp headers bs
        --Left _ -> notFound

handle' :: Req -> IO Resp
handle' = undefined

handle :: ByteString -> IO ByteString
handle bsReq =
    case runParser bsReq of
      Right req -> do
          resp <- handle' req
          pure $ toBs resp
      Left _ -> pure $ toBs notFound
