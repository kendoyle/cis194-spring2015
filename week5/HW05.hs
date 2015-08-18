{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Data.Bits
import Data.Function

import Parser

-- ghcViz

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret f1 f2 = do
    bytes1 <- BS.readFile f1
    bytes2 <- BS.readFile f2
    --let secret = (BS.dropWhile (BS.null . BS.singleton) (BS.pack $ BS.zipWith xor bytes1 bytes2))
    let secret = BS.pack . filter (/=0) $ BS.zipWith xor bytes1 bytes2
    return secret

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
    cipherBytes <- BS.readFile (path ++ ".enc")
    let plainText = BS.pack $ BS.zipWith xor cipherBytes (BS.cycle key)
    BS.writeFile path plainText

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
--fmap "every monad is also a functor"
    jsonBytes <- BS.readFile path
    let decoded = decode jsonBytes
    return decoded

-- Exercise 4 -----------------------------------------

isVictim :: Maybe [TId] -> Transaction -> Bool
isVictim Nothing _ = False
isVictim (Just ids) t = (tid t) `elem` ids

getTransactions :: Maybe[Transaction] -> [Transaction]
getTransactions Nothing = []
getTransactions (Just ts) = ts

-- fromMaybe
-- fmap <*> | :t <*>

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimFile transactionFile = do
    victims <- parseFile victimFile :: IO (Maybe[TId])
    transactions <- parseFile transactionFile :: IO(Maybe[Transaction])
    let ts = getTransactions transactions
    let vt = filter (isVictim victims) ts
    return (Just vt)

-- Exercise 5 -----------------------------------------

-- fromMaybe vs. FindWithDefault vs. Map.insertWith
getFlow :: [Transaction] -> Map String Integer
getFlow ts = 
    let flowMap = Map.empty
        getFlow' (t':ts') m = 
            let fromAmt = Map.findWithDefault 0 (from t') m - (amount t')
                toAmt = Map.findWithDefault 0 (to t') m + (amount t')
            in getFlow' ts' . Map.insert (from t') fromAmt $ Map.insert (to t') toAmt m
        getFlow' [] m = m
    in getFlow' ts flowMap

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
-- comparing vs. compare
-- max / min -> fold
getCriminal m = head . Map.keys $ Map.filter (== (last $ Map.elems m)) m

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flowMap ids = 
    let payers = reverse . sortBy (compare `on` snd) . Map.toList $ Map.filter (>0) flowMap
        payees = reverse . sortBy (compare `on` snd) . Map.toList $ Map.filter (<0) flowMap
        undoTs' (from:froms) (to:tos) (id:ids) = 
            let amt = minimum [snd from, -1 * snd to] 
                

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

