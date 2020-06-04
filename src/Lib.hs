{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Chord
import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.STM      (atomically)
import           Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVarIO,
                                              writeTVar)
import           Control.Monad               (forever)
import qualified Data.ByteString.Char8       as B
import qualified Data.Hashable               as H
import qualified Data.HashTable.IO           as HT
import qualified Network.Simple.TCP          as C
import qualified Network.Socket              as SO
import qualified System.Environment          as S

type HashTable k v = HT.LinearHashTable k v

app :: IO ()
app = do
  args <- S.getArgs
  case args of
    [] ->
      putStrLn
        "Please give two ip adresses and port numbers\n format: --a ipA pA --b ipB pB"
    ["--test", x] -> testHT (read x)
    _ -> app_ args

app_ :: [String] -> IO ()
app_ args = do
  let a = parseAddr args "--a"
  let b = parseAddr args "--b"
  hashtable a b

localIPwithPort :: Int -> Maybe (C.HostName, C.ServiceName)
localIPwithPort x = Just ("127.0.0.1", show x)

testHT :: Int -> IO ()
testHT n = do
  let central = localIPwithPort 8080
  forkIO $ hashtable central Nothing
  mapM_ (\x -> forkIO $ hashtable (localIPwithPort x) central) (enumFromTo 8081 (8080 + n))
  hashtable (localIPwithPort (8081 + n)) central

parseAddr :: [String] -> String -> Maybe (C.HostName, C.ServiceName)
parseAddr (x:ipA:pA:xs) s
  | x == s = Just (ipA, pA)
  | otherwise = parseAddr (ipA : pA : xs) s
parseAddr (_:xs) s = parseAddr xs s
parseAddr [] _ = Nothing

hashtable :: Maybe (C.HostName, C.ServiceName) -> Maybe (C.HostName, C.ServiceName) -> IO ()
hashtable a b = do
  ht <- HT.new :: IO (HashTable Int B.ByteString)
  let node = case a of
               Just val -> createNode val
               Nothing  -> createNode ("", "")
  x <- case b of
         Nothing  -> return node
         Just val -> threadDelay 1000000 >> join node val
  n <- newTVarIO x
  forkIO (forever $ refresh n)
  listenHT ht n (self node)

refresh :: TVar Node -> IO ()
refresh n = do
  node <- readTVarIO n
  putStrLn $ "refreshing node " ++ show node
  newNode <- updateNode node
  putStrLn $ "new node " ++ show newNode
  atomically (writeTVar n newNode)
  _ <- threadDelay 10000000
  return ()

lookupString :: Maybe B.ByteString -> B.ByteString
lookupString Nothing  = "not available\n"
lookupString (Just s) = B.concat ["data: ", s, "\n"]

insertString :: B.ByteString -> B.ByteString -> B.ByteString
insertString k v = B.concat ["data stored: ", k, ", ", v, "\n"]

addressString :: (C.HostName, C.ServiceName) -> B.ByteString
addressString (a, b) = let ab = B.pack a
                           bb = B.pack b in
                           B.unwords [ab, bb]

predecessorString :: Node -> B.ByteString
predecessorString n = let pred = predecessor n in
                          case pred of
                            Nothing     -> "\n"
                            Just (x, y) -> addressString (x, y)

listenHT ::
     HashTable Int B.ByteString
  -> TVar Node
  -> (C.HostName, C.ServiceName)
  -> IO ()
listenHT ht n (ah, as) =
  C.serve (C.Host ah) as $ \(connSoc, remoteAddr) -> do
    B.putStr "TCP connection established from " >> print remoteAddr
    SO.setSocketOption connSoc SO.KeepAlive 10000
    runLoop ht n connSoc

runLoop ::
     HashTable Int B.ByteString
  -> TVar Node
  -> C.Socket
  -> IO ()
runLoop ht n connSoc = do
  s <- C.recv connSoc 128
  node <- readTVarIO n
  case s of
    Nothing -> C.send connSoc "not available"
    Just val ->
      case B.words val of
        ["get", x] ->
          getValue ht node x >>= C.send connSoc >>
          runLoop ht n connSoc
        ["getID", x] ->
          (lookupString <$> HT.lookup ht (H.hashWithSalt salt x)) >>= C.send connSoc >>
          runLoop ht n connSoc
        ["put", x, y] ->
          putValue n x y >>
          runLoop ht n connSoc
        ["putID", x, y] ->
          HT.insert ht (H.hashWithSalt salt x) y >> C.send connSoc (insertString x y) >>
          runLoop ht n connSoc
        ["findSuccessor", x] ->
          findSuccessor node ((read . B.unpack) x) >>= C.send connSoc . addressString >>
          runLoop ht n connSoc
        ["predecessor"] ->
          C.send connSoc (predecessorString node) >>
          runLoop ht n connSoc
        ["notify", x, y] ->
          let pair = (B.unpack x, B.unpack y)
              new = addToFinger (notify node pair) pair in
              atomically (writeTVar n new) >>
              C.send connSoc "notified" >>
              runLoop ht n connSoc
        ["ping"] -> C.send connSoc "pong" >> runLoop ht n connSoc
        ["print"] -> C.send connSoc ((B.pack .show) node) >> runLoop ht n connSoc
        ("q":_) -> C.send connSoc "shutting the server down, bye bye"
        _ -> C.send connSoc helpString >> runLoop ht n connSoc

getValue ::
  HashTable Int B.ByteString
  -> Node
  -> B.ByteString
  -> IO B.ByteString
getValue ht n x = do
  succ <- findSuccessor n (H.hashWithSalt salt x)
  if succ == self n then lookupString <$> HT.lookup ht (H.hashWithSalt salt x) else lookupOther succ x

lookupOther :: (C.HostName, C.ServiceName) -> B.ByteString -> IO B.ByteString
lookupOther (bh, bs) x =
  C.connect bh bs $ \(connSoc, remoteAddr) -> do
    B.putStr "lookupOther: Connection established to " >> print remoteAddr
    C.send connSoc (B.unwords ["getID", x])
    s <- C.recv connSoc 10000
    case s of
      Nothing  -> return "not available"
      Just val -> return val

putValue ::
  TVar Node
  -> B.ByteString
  -> B.ByteString
  -> IO ()
putValue n k v = do
  node <- readTVarIO n
  succ <- findSuccessor node (H.hashWithSalt salt k)
  putOther succ k v

putOther :: (C.HostName, C.ServiceName) -> B.ByteString -> B.ByteString -> IO ()
putOther (bh, bs) k v =
  C.connect bh bs $ \(connSoc, remoteAddr) -> do
    B.putStr "putOther: Connection established to " >> print remoteAddr
    C.send connSoc (B.unwords ["putID", k, v])

helpString :: B.ByteString
helpString =
  "The syntax is:\n get key - To get a value\n put key value - To store values\n q - To quit"

