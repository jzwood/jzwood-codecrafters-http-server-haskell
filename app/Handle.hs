{-# LANGUAGE OverloadedStrings #-}

module Handle where

import Data.ByteString (ByteString)
import Syntax

handle :: Req -> ByteString
handle req =
    case req of
      Req{path = Path "/"} -> "HTTP/1.1 200 OK\r\n\r\n"
      Req{} -> "HTTP/1.1 404 Not Found\r\n\r\n"
