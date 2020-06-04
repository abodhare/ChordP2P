{-# LANGUAGE OverloadedStrings #-}

module Chord where

import qualified Data.ByteString.Char8 as B
import qualified Data.Hashable         as H
import qualified Data.HashTable.IO     as HT
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as MV
import qualified Network.Simple.TCP    as C
import qualified Network.Socket        as SO
import qualified System.Environment    as S

data Node = ChordNode {
  self        :: (C.HostName, C.ServiceName),
  successor   :: (C.HostName, C.ServiceName),
  predecessor :: Maybe (C.HostName, C.ServiceName),
  finger      :: V.Vector (C.HostName, C.ServiceName),
  next        :: Int
} deriving (Show)

ringBase :: Int
ringBase = 32

salt :: Int
salt = 1234

hashID :: Int -> Int
hashID x = H.hashWithSalt salt x `mod` (2 ^ ringBase)

hashIP :: (C.HostName, C.ServiceName) -> Int
hashIP x =  H.hashWithSalt salt (uncurry (++) x) `mod` (2 ^ ringBase)

between :: Int -> Int -> Int -> Bool
between a x y = let low = ((a - x) + (2 ^ ringBase)) `mod` (2 ^ ringBase)
                    high = ((y - x) + (2 ^ ringBase)) `mod` (2 ^ ringBase) in
                    low < high

findSuccessor :: Node -> Int -> IO (C.HostName, C.ServiceName)
findSuccessor n id = let succ = hashIP (successor n)
                         this = hashIP (self n) in
                         findS n id this succ where
                           findS :: Node -> Int -> Int -> Int -> IO (C.HostName, C.ServiceName)
                           findS n id this succ
                             | between id this succ || id == succ = return (successor n)
                             | otherwise = let (ah, as) = closestPrecedingNode n id in if (ah, as) == self n then return (successor n) else
                                               C.connect ah as $ \(connSoc, remoteAddr) -> do
                                               putStrLn $ "findSuccessor: Connection established to " ++ show remoteAddr
                                               C.send connSoc $ B.concat ["findSuccessor ", (B.pack . show) id]
                                               val <- C.recv connSoc 10000
                                               case val of
                                                 Nothing -> return (self n)
                                                 Just s  -> return $ let ret = convToIP s in case ret of
                                                                                               ("", "") -> self n
                                                                                               _ -> ret


convToIP :: B.ByteString -> (C.HostName, C.ServiceName)
convToIP val = case B.words val of
            [a,b] -> let ah = B.unpack a
                         bh = B.unpack b in
                         (ah, bh)
            _ -> ("", "")

closestPrecedingNode :: Node -> Int -> (C.HostName, C.ServiceName)
closestPrecedingNode n id = let this = hashIP (self n) in
                                findS n id this where
                                  findS :: Node -> Int -> Int -> (C.HostName, C.ServiceName)
                                  findS n id this = case V.find (check id this) (V.reverse (finger n)) of
                                                       Just s  -> case s of
                                                                    ("", "") -> successor n
                                                                    _ -> s
                                                       Nothing -> successor n
                                  check :: Int -> Int -> (C.HostName, C.ServiceName) -> Bool
                                  check id this m = let mh = hashIP m in between mh this id


createNode :: (C.HostName, C.ServiceName) -> Node
createNode x = ChordNode x x Nothing (V.replicate ringBase ("", "")) 0

join :: Node -> (C.HostName, C.ServiceName) -> IO Node
join n x = let succ = findS x (self n) in
               succ >>= \s -> notifySuccessor n s >> return (ChordNode (self n) s Nothing (finger n) (next n)) where
                 findS :: (C.HostName, C.ServiceName) -> (C.HostName, C.ServiceName) -> IO (C.HostName, C.ServiceName)
                 findS (xh, xs) this = C.connect xh xs $ \(connSoc, remoteAddr) -> do
                   putStrLn $ "join: Connection established to " ++ show remoteAddr
                   C.send connSoc $ B.concat ["findSuccessor ", (B.pack . show . hashIP) this]
                   val <- C.recv connSoc 10000
                   case val of
                     Nothing -> return x
                     Just s  -> return $ let ret = convToIP s in case ret of
                                                                   ("", "") -> x
                                                                   _ -> ret

addToFinger :: Node -> (C.HostName, C.ServiceName) -> Node
addToFinger n x = let index = fingerIndex (self n) x in case finger n V.!? index of
                    Nothing -> n
                    Just ("", "") -> let newFt = V.modify (\v -> MV.write v index x) (finger n) in
                                         n { finger = newFt }
                    Just _ -> n

fingerIndex :: (C.HostName, C.ServiceName) -> (C.HostName, C.ServiceName) -> Int
fingerIndex this pred = let t = hashIP this
                            p = hashIP pred
                            tupleList = reverse (zip (take ringBase (iterate (\x -> 2*(x - t) + t) (t + 1))) (enumFromTo 1 ringBase)) in
                            case filter (between p t . fst) tupleList of
                              x:_ -> snd x
                              _   -> ringBase - 1


stabilize :: Node -> IO Node
stabilize n@(ChordNode this succ@(sh, ss) predec fingerTable next) = do
              x <- getPredecessor sh ss
              putStrLn $ "stablizing with predecessor " ++ show x
              case x of
                ("", "") -> return n
                _ -> let xh = hashIP x
                         change = between xh (hashIP this) (hashIP succ) in
                         if change then
                                  let new = ChordNode this x predec fingerTable next in
                                      notifySuccessor new (successor new)
                         else
                                  let new = n in
                                      notifySuccessor new (successor new)

getPredecessor :: C.HostName -> C.ServiceName -> IO (C.HostName, C.ServiceName)
getPredecessor sh ss = C.connect sh ss $ \(connSoc, remoteAddr) -> do
                      putStrLn $ "getPredecessor: Connection established to " ++ show remoteAddr
                      C.send connSoc "predecessor"
                      val <- C.recv connSoc 10000
                      case val of
                        Nothing -> return ("", "")
                        Just s  -> return $ convToIP s


notify :: Node -> (C.HostName, C.ServiceName) -> Node
notify n@(ChordNode t s p ft next) x = case x of
        ("", "") -> n
        _ ->        case p of
                        Nothing -> ChordNode t s (Just x) ft next
                        Just pred -> let xh = hashIP x
                                         predh = hashIP pred
                                         th = hashIP t in
                                         if between xh predh th then ChordNode t s (Just x) ft next
                                                               else n

notifySuccessor :: Node -> (C.HostName, C.ServiceName) -> IO Node
notifySuccessor n (xh, xs) = C.connect xh xs $ \(connSoc, remoteAddr) -> do
                               putStrLn $ "notifySuccessor: Connection established to " ++ show remoteAddr
                               C.send connSoc $ B.concat ["notify ", ((\(a, b) -> B.unwords [B.pack a, B.pack b]) . self) n]
                               return n

fixFingers :: Node -> IO Node
fixFingers n = let nextIndex = (next n `mod` ringBase) + 1
                   succIndex = (hashIP (self n) + 2 ^ (nextIndex - 1)) `mod` ringBase in do
                   putStrLn $ "fixing fingers, nextIndex: " ++ show nextIndex ++ ", succIndex: " ++ show succIndex
                   x <- findSuccessor n succIndex
                   let newVal = if x == self n then ("", "") else x
                   let newFt = V.modify (\v -> MV.write v (nextIndex - 1) newVal) (finger n)
                   return (ChordNode (self n) (successor n) (predecessor n) newFt nextIndex)

checkPredecessor :: Node -> IO Node
checkPredecessor n = putStrLn "checking predecessor" >> case predecessor n of
                       Nothing -> return n
                       Just x -> do
                         working <- checkNode x
                         if working then return n else return n { predecessor = Nothing }

checkNode :: (C.HostName, C.ServiceName) -> IO Bool
checkNode (xh, xs) = C.connect xh xs $ \(connSoc, remoteAddr) -> do
                        putStrLn $ "checkNode: Connection established to " ++ show remoteAddr
                        C.send connSoc "ping"
                        val <- C.recv connSoc 10000
                        case val of
                          Nothing -> return False
                          Just x -> if x == "pong" then return True else return False

updateNode :: Node -> IO Node
updateNode n = stabilize n >>= checkPredecessor >>= fixFingers

