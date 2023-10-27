{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module ParserExample where

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

data Version = One | Two
data Protocol' = Protocol' { version :: Version, length :: Int, body :: ByteString }

parseVersion :: Parser Version
parseVersion = (string "1" $> One) <|> (string "2" $> Two)

parseBody :: Parser (Int, ByteString)
parseBody = digit <&> digitToInt >>= \len -> take len <&> (len,)

parseProtocol' :: Parser Protocol'
parseProtocol' = (\v (l, b) -> Protocol' v l b) <$> parseVersion <*> parseBody
