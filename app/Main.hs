{-# LANGUAGE PackageImports, OverloadedStrings, NoImplicitPrelude #-}

module Main (main) where

import Prelude ((-), (+), div, mod)

import Data.Ord ((<=))
import Data.Eq (Eq, (==))

import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Int (Int)
import Data.Bool (Bool, (&&))
import Data.Word (Word8)

import Data.List (head, last)

import System.IO (IO)

import Text.Read (Read, read)
import Text.Show (Show, show)

import Control.Applicative ((<|>))
import Control.Monad ((>>), return, unless)
import Data.Functor (fmap, (<$>))
import Data.Monoid ((<>))
import Data.Function ((.), ($))

import "base" Control.Monad.IO.Class (liftIO)

import "bytestring" Data.ByteString (ByteString)
import "bytestring" Data.ByteString.Char8
    ( pack, unpack
    , words, unwords
    , putStr, putStrLn
    )

import "attoparsec" Data.Attoparsec.ByteString
    ( Parser, parseOnly
    , string, takeWhile1, takeByteString
    )

import "conduit" Data.Conduit ((.|), runConduit)
import "conduit" Data.Conduit.Lift (evalStateLC)
import "transformers" Control.Monad.Trans.State.Lazy (StateT, get, put)

import qualified "conduit" Data.Conduit.List as C

import "conduit-extra" Data.Conduit.Binary (lines)
import "conduit-extra" Data.Conduit.Network (sourceSocket, sinkSocket)

import "network" Network.Socket
    ( AddrInfo(addrFamily, addrSocketType, addrProtocol, addrAddress)
    , getAddrInfo, defaultHints, SocketType(Stream)
    , connect, socket
    )

data Cmd a = Problem a a | Answer a | Log ByteString deriving (Show, Eq)

toMaybe :: Either a b -> Maybe b
toMaybe (Left _) = Nothing
toMaybe (Right a) = Just a

main :: IO ()
main = do
    addr:_ <- getAddrInfo
        (Just defaultHints {addrSocketType = Stream})
        (Just "pwnable.kr") (Just "9007")

    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)

    runConduit $ sourceSocket sock
        .| lines
        .| C.map (toMaybe . parseOnly parser)
        .| evalStateLC (0, 0) (C.mapM (maybe (return "") runner))
        .| C.mapM (\x -> do
            unless (x == "") $ do
                let xs = words x
                putStr ("-> " <> head xs <> ".." <> last xs <> " ")
            return x)
        .| sinkSocket sock

isDigit :: Word8 -> Bool
isDigit x = 0x30 <= x && x <= 0x39
 
question, answer :: Read a => Parser (Cmd a)
question = do
    _ <- string "N="
    n <- takeWhile1 isDigit
    _ <- string " C="
    c <- takeWhile1 isDigit
    return $ Problem (read $ unpack n) (read $ unpack c)

answer = (Answer . read . unpack) <$> takeWhile1 isDigit

log :: Parser (Cmd a)
log = Log <$> takeByteString

parser :: Parser (Cmd Int)
parser = question <|> answer <|> log

fz :: Int -> Int -> ByteString
fz min max = (unwords . fmap (pack . show) $ [min .. max]) <> "\n"

runner :: Cmd Int -> StateT (Int, Int) IO ByteString
runner (Log str) = liftIO (putStrLn str) >> return ""

runner (Problem n _) =
    let max = n-1
        mid = max `div` 2
    in put (0, max) >> return (fz 0 mid)

runner (Answer x) = do
    (min, max) <- get

    let mid = (min + max) `div` 2
        midL = (min + mid) `div` 2
        midR = (mid+1 + max) `div` 2

    if min == max
        then return $ pack (show min) <> "\n"
        else if (x `mod` 10) == 9
            then put (min,   mid) >> return (fz min     midL)
            else put (mid+1, max) >> return (fz (mid+1) midR)
