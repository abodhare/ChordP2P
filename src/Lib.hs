{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( app
  ) where

import           Chord
import qualified Data.ByteString.Char8 as B
import qualified Data.Hashable         as H
import qualified Data.HashTable.IO     as HT
import qualified Network.Simple.TCP    as C
import qualified Network.Socket        as SO
import qualified System.Environment    as S

type HashTable k v = HT.LinearHashTable k v

app :: IO ()
app = do
  args <- S.getArgs
  case args of
    [] ->
      putStrLn
        "Please give two ip adresses and port numbers\n format: --a ipA pA --b ipB pB"
    _ -> app_ args

app_ :: [String] -> IO ()
app_ args = do
  let a = parseAddr args "--a"
  let b = parseAddr args "--b"
  hashtable a b

parseAddr :: [String] -> String -> (C.HostName, C.ServiceName)
parseAddr (x:ipA:pA:xs) s
  | x == s = (ipA, pA)
  | otherwise = parseAddr (ipA : pA : xs) s
parseAddr (_:xs) s = parseAddr xs s
parseAddr [] _ = ("", "")

hashtable :: (C.HostName, C.ServiceName) -> (C.HostName, C.ServiceName) -> IO ()
hashtable a b = do
  ht <- HT.new :: IO (HashTable B.ByteString B.ByteString)
  listenHT ht a b

lookupString :: Maybe B.ByteString -> B.ByteString
lookupString Nothing  = "not available\n"
lookupString (Just s) = B.concat ["data: ", s, "\n"]

insertString :: B.ByteString -> B.ByteString -> B.ByteString
insertString k v = B.concat ["data stored: ", k, ", ", v, "\n"]

listenHT ::
     HashTable B.ByteString B.ByteString
  -> (C.HostName, C.ServiceName)
  -> (C.HostName, C.ServiceName)
  -> IO ()
listenHT ht (ah, as) b =
  C.serve (C.Host ah) as $ \(connSoc, remoteAddr) -> do
    B.putStr "TCP connection established from " >> print remoteAddr
    SO.setSocketOption connSoc SO.KeepAlive 10000
    let x = createNode (ah, as)
    join x b
    runLoop ht b connSoc

runLoop ::
     HashTable B.ByteString B.ByteString
  -> (C.HostName, C.ServiceName)
  -> C.Socket
  -> IO ()
runLoop ht b connSoc = do
  s <- C.recv connSoc 128
  case s of
    Nothing -> C.send connSoc "not available"
    Just val ->
      case B.words val of
        ["get", x] ->
          HT.lookup ht x >>= lookupOther b x >>= C.send connSoc >>
          runLoop ht b connSoc
        ["getID", x] ->
          (lookupString <$> HT.lookup ht x) >>= C.send connSoc >>
          runLoop ht b connSoc
        ["put", x, y] ->
          HT.insert ht x y >> C.send connSoc (insertString x y) >>
          runLoop ht b connSoc
        ("q":_) -> C.send connSoc "shutting the server down, bye bye"
        _ -> C.send connSoc helpString >> runLoop ht b connSoc

lookupOther ::
     (C.HostName, C.ServiceName)
  -> B.ByteString
  -> Maybe B.ByteString
  -> IO B.ByteString
lookupOther b x Nothing    = lookupOther_ b x
lookupOther _ _ s@(Just x) = return $ lookupString s

lookupOther_ :: (C.HostName, C.ServiceName) -> B.ByteString -> IO B.ByteString
lookupOther_ (bh, bs) x =
  C.connect bh bs $ \(connSoc, remoteAddr) -> do
    B.putStr "Connection established to " >> print remoteAddr
    C.send connSoc (B.unwords ["getID", x])
    s <- C.recv connSoc 128
    case s of
      Nothing  -> return "not available"
      Just val -> return val

helpString :: B.ByteString
helpString =
  "The syntax is:\n get key - To get a value\n put key value - To store values\n q - To quit"
