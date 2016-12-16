{-# LANGUAGE PackageImports, OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class (liftIO)

import "bytestring" Data.ByteString (ByteString)
import qualified "bytestring" Data.ByteString.Char8 as BSC (putStrLn)

import "conduit" Data.Conduit
    ( Conduit
    , runConduit, fuse, await, yield
    )

import qualified "conduit-extra" Data.Conduit.Binary as B (lines)
import "conduit-extra" Data.Conduit.Network (sourceSocket, sinkSocket)
import qualified "conduit" Data.Conduit.List as L

import "network" Network.Socket
    ( AddrInfo(addrFamily, addrSocketType, addrProtocol, addrAddress)
    , getAddrInfo, defaultHints, SocketType(Stream)
    , close, connect, socket
    )

main :: IO ()
main = do
    addr:_ <- getAddrInfo (Just defaultHints {addrSocketType = Stream})
                          (Just "pwnable.kr") (Just "9007")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)

    runConduit (sourceSocket sock `fuse` B.lines `fuse` L.mapM (\x -> BSC.putStrLn x >> return x) `fuse` sinkSocket sock)
