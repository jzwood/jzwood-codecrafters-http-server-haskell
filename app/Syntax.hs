module Syntax where

import Data.ByteString (ByteString)

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
    }
    deriving (Eq, Show)

data Resp = Resp
    { protocol' :: Protocol
    , status :: Status
    , headers :: [ByteString]
    , body :: ByteString
    }
    deriving (Eq, Show)