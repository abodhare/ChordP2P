{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Lib
    ( app
    ) where

import qualified Data.ByteString.Char8 as B 
import qualified Data.HashTable.IO as HT
import qualified Data.Hashable as H
import qualified Network.Simple.TCP as C
import qualified Network.Socket as SO
import qualified System.Environment as S

type HashTable k v = HT.LinearHashTable k v

app :: IO ()
app = do
    args <- S.getArgs
    case args of
        [] -> putStrLn "Please give two ip adresses and port numbers\n format: --a ipA pA --b ipB pB"
        _ -> app_ args

app_ :: [String] -> IO ()
app_ args = do
    a <- parseAddr args
    hashtable a

parseAddr :: [String] -> IO (C.HostName, C.ServiceName)
parseAddr ("--addr" : ipA : pA : _) = return (ipA, pA)
parseAddr (_:xs) = parseAddr xs
parseAddr [] = return ("","")

hashtable :: (C.HostName, C.ServiceName) -> IO ()
hashtable a@(ah, ap) = do
    ht <- HT.new :: IO (HashTable B.ByteString B.ByteString)
    listenHT ht a

lookupHT :: HashTable B.ByteString B.ByteString -> B.ByteString -> IO B.ByteString
lookupHT ht x = HT.lookup ht x >>= (\case Nothing -> return "not available\n"
                                          Just s -> return (B.concat ["data: ", s, "\n"]))

insertHT :: HashTable B.ByteString B.ByteString -> B.ByteString -> B.ByteString -> IO B.ByteString
insertHT ht k v = HT.insert ht k v >> return (B.concat ["data stored: ", k, ", ", v, "\n"])

listenHT :: HashTable B.ByteString B.ByteString -> (C.HostName, C.ServiceName) -> IO ()
listenHT ht (ah, as) = C.serve (C.Host ah) as $ \(connSoc, remoteAddr) -> do
    B.putStr "TCP connection established from " >> print remoteAddr
    SO.setSocketOption connSoc SO.KeepAlive 10000
    runLoop ht connSoc

runLoop :: HashTable B.ByteString B.ByteString -> C.Socket -> IO ()
runLoop ht connSoc = do
    s <- C.recv connSoc 128
    case s of
        Nothing -> C.send connSoc "not available"
        Just val -> case B.words val of
                        ["get", x] -> lookupHT ht x >>= C.send connSoc >> runLoop ht connSoc
                        ["put", x, y] -> insertHT ht x y >>= C.send connSoc >> runLoop ht connSoc
                        ("q":_) -> C.send connSoc "shutting the server down, bye bye"
                        _          -> C.send connSoc helpString >> runLoop ht connSoc

helpString :: B.ByteString
helpString = "The syntax is:\n get key - To get a value\n put key value - To store values\n q - To quit"
