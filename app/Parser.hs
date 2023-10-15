{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Syntax

import Data.Functor
import Control.Applicative
import Data.Attoparsec.ByteString.Char8 (Parser, endOfLine, isSpace, parseOnly, skipSpace, space, string, take, takeTill, many', takeWhile1, takeByteString)
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

parseLine :: Parser ByteString
parseLine = takeTill (=='\r') <* endOfLine

parseHeader :: Parser KeyVal
parseHeader = liftA2 (,) (takeTill (==':') <* string ":" <* skipSpace) parseLine

parseHeaders :: Parser Map
parseHeaders = many' parseHeader

parseReq :: Parser Req
parseReq =
    Req <$> parseMethod
        <* space
        <*> parsePath
        <* space
        <*> parseProtocol
        <*> parseHeaders

runParser :: ByteString -> Either String Req
runParser = parseOnly parseReq
