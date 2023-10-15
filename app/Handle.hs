{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Handle (handle) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Functor

--import Data.Attoparsec.ByteString.Char8 (Parser, char8, count, decimal, digit, endOfLine, isSpace, parseOnly, skipSpace, space, string, take, takeTill)
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, string, takeByteString, endOfInput)
import qualified Data.ByteString as B
import Format
import Syntax

toHeader :: ByteString -> ByteString -> (ByteString, ByteString)
toHeader = (,)

ok :: ByteString -> Resp
ok body =
    Resp
        { protocol' = HTTP1_1
        , status = Status 200
        , headers' = [ toHeader "Content-Type" "text/plain"
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

parsePath :: Parser ByteString
parsePath =  (string "/" *> endOfInput $> "")
         <|> (string "/echo/" *> takeByteString)
         <|> (string "/echo/" *> takeByteString)

handle :: Req -> ByteString
handle Req{path = (Path path)} =
    toBs $
        case parseOnly parsePath path of
          Right bs -> ok bs
          Left _ -> notFound
