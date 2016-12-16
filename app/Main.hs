{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding (log, takeWhile, putStrLn, min, max, unwords, lines)

import Network.Socket (close, connect, socket, getAddrInfo, defaultHints, SocketType(Stream), AddrInfo(..))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack, unwords, putStrLn)
import Data.Conduit.Network (sourceSocket, sinkSocket)
import Data.Conduit.Binary (lines)
import Data.Conduit ((=$=), ($$), (.|), Conduit, await, yield, runConduit)
import Data.Conduit.Lift (stateLC, evalStateLC)
import qualified Data.Conduit.List as C
import Data.Monoid ((<>))
import Data.Attoparsec.ByteString (string, Parser, takeWhile1, takeByteString, parseOnly)
import Control.Applicative ((<|>))
import Control.Monad.Trans.State.Lazy (StateT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (join)

data Cmd a = Problem a a | Answer a | Log ByteString

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

isDigit :: Integral a => a -> Bool
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

fz :: (Show a, Enum a) => a -> a -> ByteString
fz min max = (unwords . map (pack . show) $ [min .. max]) <> "\n"
