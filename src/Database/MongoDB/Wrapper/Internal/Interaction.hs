{-# LANGUAGE OverloadedStrings #-}

module Database.MongoDB.Wrapper.Internal.Interaction
  ( closePipe
  , deleteFromDB
  , getAllS
  , getFromDB
  , getPipe
  , insertAllDB
  , putIntoDB, putIntoDBPipe
  ) where

import           Control.Arrow                                        ((&&&))
import           Control.DeepSeq                                      (NFData,
                                                                       force)
import           Control.Exception                                    (evaluate)
import           Control.Monad                                        (join)
import           Data.Aeson                                           (FromJSON,
                                                                       ToJSON,
                                                                       fromJSON,
                                                                       toJSON)
import           Data.Aeson.Types                                     (Result (..))
import qualified Data.Bson                                            as B (Value (..))
import           Data.Either.Combinators                              (fromRight')
import           Data.Text                                            (Text)
import           Database.MongoDB                                     (Document,
                                                                       Field,
                                                                       Pipe,
                                                                       access,
                                                                       close,
                                                                       connect,
                                                                       deleteAll,
                                                                       find,
                                                                       host,
                                                                       insertAll_,
                                                                       insert_,
                                                                       limit,
                                                                       master,
                                                                       rest,
                                                                       select)
import           Database.MongoDB.Wrapper.Internal.AesonBsonConverter (fromDocument,
                                                                       toBson)
import           System.BCD.Config.Mongo                              (FromJsonConfig (..),
                                                                       MongoConfig (..))

dbHost :: IO String
dbHost = fmap _host fromJsonConfig

putIntoDB :: ToJSON a => Text -> Text -> a -> IO ()
putIntoDB dataBaseName collectionName obj = do
  pipe <- join (connect <$> fmap host dbHost)
  access pipe master dataBaseName (insertAll_ collectionName [fromDoc . toBson . toJSON $ obj])
  close pipe

putIntoDBPipe :: ToJSON a => Pipe -> Text -> Text -> a -> IO ()
putIntoDBPipe pipe dataBaseName collectionName obj =
  access pipe master dataBaseName (insert_ collectionName (fromDoc . toBson . toJSON $ obj))

getFromDB :: FromJSON a => [Field] -> Text -> Text -> IO (Either String a)
getFromDB selector dataBaseName collectionName = do
  pipe <- join (connect <$> fmap host dbHost)
  items <- access pipe master dataBaseName $ find (select selector collectionName) >>= rest
  close pipe
  case items of
    []      -> return (Left "No match for query in database.")
    (x : _) -> return ((fromResult . fromJSON . fromDocument) x)

insertAllDB :: ToJSON a => Text -> Text -> [a] -> IO ()
insertAllDB dataBaseName collectionName items = do
  pipe <- join (connect <$> fmap host dbHost)
  access pipe master dataBaseName $ insertAll_ collectionName (fmap (fromDoc . toBson . toJSON) items)
  close pipe

getPipe :: IO Pipe
getPipe = join (connect <$> fmap host dbHost)

closePipe :: Pipe -> IO ()
closePipe = close

deleteFromDB :: [[Field]] -> Text -> Text -> IO ()
deleteFromDB selectors dataBaseName collectionName = do
  pipe <- join (connect <$> fmap host dbHost)
  _ <- access pipe master dataBaseName (deleteAll collectionName (fmap (id &&& const []) selectors))
  close pipe

getAllS :: (FromJSON a, NFData a) => Int -> Text -> Text -> [Field] -> IO [a]
getAllS toLoad dBName collName selection = do
  pipe <- join (connect <$> fmap host dbHost)
  resS <- access pipe master dBName $ find ((select selection collName) {limit = fromIntegral toLoad}) >>= rest
  close pipe
  base <- mapM (evaluate . force . fromRight' . fromResult . fromJSON . fromDocument) resS
  return base

fromResult:: FromJSON a => Result a -> Either String a
fromResult (Error msg)      = Left ("*** Error: extracting DBUnit failed.\n" ++ msg)
fromResult (Success dBUnit) = Right dBUnit

fromDoc :: B.Value -> Document
fromDoc (B.Doc document) = document
fromDoc _                = error "*** Error: extracting Document failed.\n"
