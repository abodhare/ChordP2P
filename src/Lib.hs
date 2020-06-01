{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( app
  ) where

import           Chord
import           Control.Concurrent    (forkIO, threadDelay)
import           Control.Monad         (forever)
import qualified Data.ByteString.Char8 as B
import qualified Data.Hashable         as H
import qualified Data.HashTable.IO     as HT
import           Data.IORef
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
  x <- join (createNode a) b
  n <- newIORef x
  forkIO (forever $ refresh n)
  listenHT ht n a

refresh :: IORef Node -> IO ()
refresh n = do
  node <- readIORef n
  newNode <- updateNode node
  writeIORef n newNode
  _ <- threadDelay 10000
  return ()

lookupString :: Maybe B.ByteString -> B.ByteString
lookupString Nothing  = "not available\n"
lookupString (Just s) = B.concat ["data: ", s, "\n"]

insertString :: B.ByteString -> B.ByteString -> B.ByteString
insertString k v = B.concat ["data stored: ", k, ", ", v, "\n"]

addressString :: (C.HostName, C.ServiceName) -> B.ByteString
addressString (a, b) = let ab = (read . show) a
                           bb = (read . show) b in
                           B.unwords [ab, bb]

predecessorString :: Node -> B.ByteString
predecessorString n = let pred = predecessor n in
                          case pred of
                            Nothing     -> ""
                            Just (x, y) -> addressString (x, y)

listenHT ::
     HashTable B.ByteString B.ByteString
  -> IORef Node
  -> (C.HostName, C.ServiceName)
  -> IO ()
listenHT ht n (ah, as) =
  C.serve (C.Host ah) as $ \(connSoc, remoteAddr) -> do
    B.putStr "TCP connection established from " >> print remoteAddr
    SO.setSocketOption connSoc SO.KeepAlive 10000
    runLoop ht n connSoc

runLoop ::
     HashTable B.ByteString B.ByteString
  -> IORef Node
  -> C.Socket
  -> IO ()
runLoop ht n connSoc = do
  s <- C.recv connSoc 128
  node <- readIORef n
  case s of
    Nothing -> C.send connSoc "not available"
    Just val ->
      case B.words val of
        ["get", x] ->
          getValue node x >>= C.send connSoc >>
          runLoop ht n connSoc
        ["getID", x] ->
          (lookupString <$> HT.lookup ht x) >>= C.send connSoc >>
          runLoop ht n connSoc
        ["put", x, y] ->
          putValue n x y >>
          runLoop ht n connSoc
        ["putID", x, y] ->
          HT.insert ht x y >> C.send connSoc (insertString x y) >>
          runLoop ht n connSoc
        ["findSuccessor", x] ->
          findSuccessor node ((read . show) x) >>= C.send connSoc . addressString >>
          runLoop ht n connSoc
        ["predecessor"] ->
          C.send connSoc (predecessorString node) >>
          runLoop ht n connSoc
        ["notify", x, y] ->
          let new = notify node (show x, show y) in
              writeIORef n new >>
              C.send connSoc "notified" >>
              runLoop ht n connSoc
        ["ping"] -> C.send connSoc "pong" >> runLoop ht n connSoc
        ["print"] -> C.send connSoc ((read .show) node) >> runLoop ht n connSoc
        ("q":_) -> C.send connSoc "shutting the server down, bye bye"
        _ -> C.send connSoc helpString >> runLoop ht n connSoc

getValue ::
  Node
  -> B.ByteString
  -> IO B.ByteString
getValue n x = do
  succ <- findSuccessor n (H.hash x)
  lookupOther succ x

lookupOther :: (C.HostName, C.ServiceName) -> B.ByteString -> IO B.ByteString
lookupOther (bh, bs) x =
  C.connect bh bs $ \(connSoc, remoteAddr) -> do
    B.putStr "Connection established to " >> print remoteAddr
    C.send connSoc (B.unwords ["getID", x])
    s <- C.recv connSoc 128
    case s of
      Nothing  -> return "not available"
      Just val -> return val

putValue ::
  IORef Node
  -> B.ByteString
  -> B.ByteString
  -> IO ()
putValue n k v = do
  node <- readIORef n
  succ <- findSuccessor node (H.hash k)
  putOther succ k v

putOther :: (C.HostName, C.ServiceName) -> B.ByteString -> B.ByteString -> IO ()
putOther (bh, bs) k v =
  C.connect bh bs $ \(connSoc, remoteAddr) -> do
    B.putStr "Connection established to " >> print remoteAddr
    C.send connSoc (B.unwords ["putID", k, v])

helpString :: B.ByteString
helpString =
  "The syntax is:\n get key - To get a value\n put key value - To store values\n q - To quit"

