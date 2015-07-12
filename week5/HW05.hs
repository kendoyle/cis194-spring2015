{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import Data.Bits (xor)
import Control.Applicative ((<*>))
import Data.Maybe (fromMaybe)

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret originalFile modifiedFile = do
  original <- BS.readFile originalFile
  modified <- BS.readFile modifiedFile
  return . BS.pack . filter (/= 0) $ BS.zipWith xor original modified

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey k f = do
  cipherText <- BS.readFile (f ++ ".enc")
  let plainText = BS.pack $ BS.zipWith xor cipherText (BS.cycle k)
  BS.writeFile f plainText

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile jsonFile = fmap decode (BS.readFile jsonFile)

-- Exercise 4 -----------------------------------------

badTs :: [TId] -> [Transaction] -> [Transaction]
badTs tids = filter ((`elem` tids) . tid)

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsFile transactionsFile = do
  victims <- parseFile victimsFile :: IO (Maybe [TId])
  transactions <- parseFile transactionsFile :: IO (Maybe [Transaction])
  return $ fmap badTs victims <*> transactions

-- Exercise 5 -----------------------------------------

insertWithOp :: (Transaction -> String) -> (Integer -> Integer -> Integer) ->
                Transaction -> Map String Integer -> Map String Integer
insertWithOp proj op t m = Map.insert (proj t) (v `op` amount t) m
  where v = fromMaybe 0 (Map.lookup (proj t) m)

insertTo :: Transaction -> Map String Integer -> Map String Integer
insertTo = insertWithOp to (+)

insertFrom :: Transaction -> Map String Integer -> Map String Integer
insertFrom = insertWithOp from (-)

insertTrans :: Transaction -> Map String Integer -> Map String Integer
insertTrans t m = insertFrom t (insertTo t m)

getFlow :: [Transaction] -> Map String Integer
getFlow = foldr insertTrans Map.empty

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m = fst $ Map.foldrWithKey f ("", 0) m
  where f k a b = if a > snd b then (k,a) else b

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

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

