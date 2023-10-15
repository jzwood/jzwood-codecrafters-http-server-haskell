{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Syntax

import Data.Functor
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (Parser, char8, count, decimal, digit, endOfLine, isSpace, parseOnly, skipSpace, space, string, take, takeTill)
import Data.ByteString (ByteString)

parseMethod :: Parser Method
parseMethod = Method <$> (string "GET" <|> string "POST")

parsePath :: Parser Path
parsePath = Path <$> takeTill isSpace

parseProtocol :: Parser Protocol
parseProtocol =
    (string "HTTP/1.0" $> HTTP1_0)
        <|> (string "HTTP/1.1" $> HTTP1_1)
        <|> (string "HTTP/2.0" $> HTTP2_0)

parseReq :: Parser Req
parseReq =
    Req <$> parseMethod
        <* space
        <*> parsePath
        <* space
        <*> parseProtocol

runParser :: ByteString -> Either String Req
runParser = parseOnly parseReq
