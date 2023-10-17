{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromMaybe)
import Handle
import Format
import Network.Simple.TCP (HostPreference (..), recv, send, serve)
import Parser

main :: IO ()
main = do
    BLC.putStrLn "Logs from your program will appear here"

    let host = "127.0.0.1"
        port = "4221"

    BLC.putStrLn $ "Listening on " <> BLC.pack host <> ":" <> BLC.pack port
    serve (Host host) port $ \(serverSocket, serverAddr) ->
        do
            BLC.putStrLn $ "Accepted connection from " <> BLC.pack (show serverAddr) <> "."
            mReq <- recv serverSocket 1024
            let bsReq = fromMaybe B.empty mReq
            bsRes <- handle bsReq
            --print bsReq
            --print bsRes
            send serverSocket bsRes
