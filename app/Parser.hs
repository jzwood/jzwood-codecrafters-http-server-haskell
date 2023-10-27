{-# LANGUAGE OverloadedStrings #-}

module Parser (parseReq, runParser) where

import Syntax

import Data.Char (digitToInt)
import Control.Applicative
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8 (
    Parser,
    endOfLine,
    isSpace,
    many',
    parseOnly,
    skipSpace,
    space,
    string,
    take,
    takeTill,
    digit
 )
import Data.ByteString (ByteString)
import Data.Functor
import Prelude hiding (take)

parseMethod :: Parser Method
parseMethod =
    (string "GET" $> GET)
        <|> (string "POST" $> POST)

parsePath :: Parser Path
parsePath = Path <$> takeTill isSpace

parseProtocol :: Parser Protocol
parseProtocol =
    (string "HTTP/1.0" $> HTTP1_0)
        <|> (string "HTTP/1.1" $> HTTP1_1)
        <|> (string "HTTP/2.0" $> HTTP2_0)

parseLine :: Parser ByteString
parseLine = takeTill (== '\r') <* endOfLine

parseHeader :: Parser KeyVal
parseHeader = liftA2 (,) (takeTill (== ':') <* string ":" <* skipSpace) parseLine

parseHeaders :: Parser Map
parseHeaders = many' parseHeader

parseHeadersAndBody :: Parser (Map, Body)
parseHeadersAndBody = parseHeaders >>= \h -> take 160 <&> \body -> (h, Body body)

parseReq :: Parser Req
parseReq =
    (\m pa pr (he, bo) -> Req m pa pr he bo) <$> parseMethod
        <* space
        <*> parsePath
        <* space
        <*> parseProtocol
        <* endOfLine
        <*> parseHeadersAndBody
        <* endOfLine

runParser :: ByteString -> Either String Req
runParser = parseOnly parseReq
