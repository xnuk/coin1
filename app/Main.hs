{-# LANGUAGE PackageImports, OverloadedStrings, NoImplicitPrelude #-}

module Main (main) where

import Prelude ((-), (+), div, mod)

import Data.Ord ((<=))
import Data.Eq (Eq, (==))

import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Int (Int)
import Data.Bool (Bool, (&&))
import Data.Word (Word8)

import System.IO (IO, print)

import Text.Read (Read, read)
import Text.Show (Show, show)

import Control.Applicative ((<|>))
import Control.Monad (join, (>>), return)
import Data.Functor (fmap, (<$>))
import Data.Monoid ((<>))
import Data.Function ((.), ($))

import "base" Control.Monad.IO.Class (liftIO)

import "bytestring" Data.ByteString (ByteString)
import "bytestring" Data.ByteString.Char8
    ( pack, unpack
    , unwords
    , putStrLn, getLine
    )

import "attoparsec" Data.Attoparsec.ByteString
    ( Parser, parseOnly
    , string, takeWhile1, takeByteString
    )

import "conduit" Data.Conduit
    ( Conduit
    , (=$=), ($$), await, yield
    )
import "conduit" Data.Conduit.Lift (stateLC, evalStateLC)
import "transformers" Control.Monad.Trans.State.Lazy (StateT)

import qualified "conduit" Data.Conduit.List as C

import "conduit-extra" Data.Conduit.Binary (lines)
import "conduit-extra" Data.Conduit.Network (sourceSocket, sinkSocket)

import "network" Network.Socket
    ( AddrInfo(addrFamily, addrSocketType, addrProtocol, addrAddress)
    , getAddrInfo, defaultHints, SocketType(Stream)
    , close, connect, socket
    )

data Cmd a = Problem a a | Answer a | Log ByteString deriving (Show, Eq)

toMaybe :: Either a b -> Maybe b
toMaybe (Left _) = Nothing
toMaybe (Right a) = Just a

main :: IO ()
main = do
    addr:_ <- getAddrInfo (Just defaultHints {addrSocketType = Stream}) (Just "pwnable.kr") (Just "9007")
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)

    sourceSocket sock
        =$= lines
        =$= logger
        =$= C.map (toMaybe . parseOnly parser)
        =$= evalStateLC (0, 0, 0) runner
        $$ sinkSocket sock

    getLine >> close sock

isDigit :: Word8 -> Bool
isDigit x = 0x30 <= x && x <= 0x39
 
question, answer :: Read a => Parser (Cmd a)
question = do
    string "N="
    n <- takeWhile1 isDigit
    string " C="
    c <- takeWhile1 isDigit
    return $ Problem (read $ unpack n) (read $ unpack c)

answer = (Answer . read . unpack) <$> takeWhile1 isDigit

log :: Parser (Cmd a)
log = Log <$> takeByteString

parser :: Read a => Parser (Cmd a)
parser = question <|> answer <|> log

logger :: Show a => Conduit a IO a
logger = do
    a <- C.head

    case a of
        Just x -> do
            liftIO $ print x
            yield x
        Nothing -> return ()

runner :: Conduit (Maybe (Cmd Int)) (StateT (Int, Int, Int) IO) ByteString
runner = do
    a <- await
    case join a of
        Nothing -> return ()
        Just (Log str) -> liftIO $ putStrLn str
        Just (Problem n _) -> stateLC $ \_ ->
            let max = n-1
                mid = max `div` 2
            in yield (fz 0 mid) >> return ((), (0, mid, max))
        Just (Answer x) ->
            if (x `mod` 10) == 0
                then stateLC $ \(_, m, max) ->
                    let min = m+1
                        mid = (min + max) `div` 2
                    in yield (fz min max) >> return ((), (min, mid, max))
                else stateLC $ \(min, m, _) ->
                    let max = m
                        mid = (min + max) `div` 2
                    in yield (fz min max) >> return ((), (min, mid, max))

fz :: Int -> Int -> ByteString
fz min max = (unwords . fmap (pack . show) $ [min .. max]) <> "\n"
