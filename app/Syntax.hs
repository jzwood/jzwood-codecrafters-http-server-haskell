module Syntax where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Functor
import Data.Function
import Data.Maybe (fromMaybe)
import Data.List (lookup)

type KeyVal = (ByteString, ByteString)
type Map = [KeyVal]

getHeader :: ByteString -> Map -> ByteString
getHeader key headers = lookup key headers & fromMaybe B.empty

newtype Method = Method ByteString
    deriving (Eq, Show)

newtype Path = Path ByteString
    deriving (Eq, Show)

newtype Status = Status Integer
    deriving (Eq, Show)

data Protocol = HTTP1_0 | HTTP1_1 | HTTP2_0
    deriving (Eq, Show)

data Req = Req
    { method :: Method
    , path :: Path
    , protocol :: Protocol
    , headers :: Map
    }
    deriving (Eq, Show)

data Resp = Resp
    { protocol' :: Protocol
    , status :: Status
    , headers' :: Map
    , body :: ByteString
    }
    deriving (Eq, Show)
