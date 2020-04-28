{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Lib
    ( someFunc
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as I
import qualified Data.HashTable.IO as HT
import qualified Data.Hashable as H

type HashTable k v = HT.LinearHashTable k v

someFunc :: IO ()
someFunc = do
    ht <- HT.new :: IO (HashTable T.Text T.Text)
    ask ht
    return ()

ask :: HashTable T.Text T.Text -> IO (HashTable T.Text T.Text)
ask ht = do 
    x <- I.getLine
    case T.words x of
        ["get", y] -> lookupHT ht y >>= (\case Nothing -> I.putStrLn "Didn't find it"
                                               Just s -> I.putStrLn (T.append "You wanted " s)) >> ask ht
        ["put", k, v] -> insertHT ht k v >> I.putStrLn (T.concat ["You stored ", k, " ", v]) >> ask ht
        ["q"] -> I.putStrLn "Bye Bye" >> return ht
        _ -> I.putStrLn helpString >> ask ht

lookupHT :: (Eq k, H.Hashable k) => HashTable k v -> k -> IO (Maybe v)
lookupHT = HT.lookup

insertHT :: (Eq k, H.Hashable k) => HashTable k v -> k -> v -> IO ()
insertHT = HT.insert

helpString :: T.Text
helpString = "The syntax is:\n get key - To get a value\n put key value - To store values\n q - To quit"
