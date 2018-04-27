{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.MongoDB.WrapperNew
  (
    MongoPool (..)
  , FromJsonConfig (..)
  , loadMongoPipe
  , loadMongoPool
  , withMongoPool
  , encode
  , decode
  , find
  , findAll
  ) where

import           Control.Monad.IO.Class                               (MonadIO)
import           Control.Monad.Trans.Control                          (MonadBaseControl (..))
import           Data.Aeson                                           (FromJSON (..),
                                                                       ToJSON (..),
                                                                       fromJSON,
                                                                       toJSON)
import           Data.Aeson.Types                                     (Result (..))
import qualified Data.Bson                                            as B (Value (..))
import           Data.Map.Strict                                      (findWithDefault)
import           Data.Pool                                            (Pool, createPool,
                                                                       withResource)
import           Data.Text                                            (Text,
                                                                       pack)
import qualified Database.MongoDB                                     as DB
import           Database.MongoDB.Wrapper.Internal.AesonBsonConverter (fromDocument,
                                                                       toBson)
import           System.BCD.Config.Mongo                              (FromJsonConfig (..),
                                                                       MongoConfig (..))

-- | 'MongoPool' contains information about 'Database' name and 'Pool' with 'Pipe's.
--
data MongoPool = MongoPool { database :: DB.Database  -- ^ name of the database
                           , pool     :: Pool DB.Pipe -- ^ pool of pipes to the database
                           }

-- | Loads 'Host' from config.json.
--
instance FromJsonConfig DB.Host where
  fromJsonConfig = do
      MongoConfig{..} <- fromJsonConfig
      pure . DB.readHostPort $ _host ++ ":" ++ show _port

-- | Loads settings from config.json and creates 'Pipe'.
--
loadMongoPipe :: DB.Database -> DB.Username -> DB.Password -> IO DB.Pipe
loadMongoPipe database' user' password' = do
    host' <- fromJsonConfig
    DB.connect host' >>= testAccess
  where
    testAccess :: DB.Pipe -> IO DB.Pipe
    testAccess pipe = DB.access pipe DB.UnconfirmedWrites database' (DB.auth user' password') >> pure pipe

-- | Loads 'MongoPool' from config.json.
--
loadMongoPool :: String -> IO MongoPool
loadMongoPool databaseVar = do
    MongoConfig{..} <- fromJsonConfig
    let database = findWithDefault (error $ "Mongo database could not be found by var: " ++ databaseVar) databaseVar _databases
    let connect = loadMongoPipe database (pack _user) (pack _password)
    -- 4, 30 and 1 some defaults values for stripes, timeout and resource per second
    MongoPool database <$> createPool connect DB.close 4 30 1

-- | Takes 'MongoPool' and runs the 'Action'.
--
withMongoPool :: (MonadBaseControl IO m, MonadIO m) => MongoPool -> DB.Action m a -> m a
withMongoPool (MongoPool database pool) action = withResource pool run
  where
    run pipe = DB.access pipe DB.master database action

-- | Encodes from object to 'Document'.
--
encode :: ToJSON a => a -> DB.Document
encode = fromDoc . toBson . toJSON

-- | Decodes from 'Document' to either object.
--
decode :: FromJSON a => DB.Document -> Either String a
decode = fromResult . fromJSON . fromDocument

-- | Redefine find function from Database.MongoDB.
--
find :: MonadIO m => Int -> Text -> [DB.Field] -> DB.Action m [DB.Document]
find count collection selection = DB.find ((DB.select selection collection) {DB.limit = fromIntegral count}) >>= DB.rest

-- | Find for given collection and selectors.
--
findAll :: MonadIO m => Text -> [DB.Field] -> DB.Action m [DB.Document]
findAll = find 0

----------------------
-- INTERNAL
----------------------

-- | Takes result from converting from JSON and converts it into 'Either'.
--
fromResult :: FromJSON a => Result a -> Either String a
fromResult (Error msg)      = Left ("*** Database.MongoDB.Wrapper error: extracting DBUnit failed.\n" ++ msg)
fromResult (Success dBUnit) = Right dBUnit

-- | Converts BSON Value to the Document.
--
fromDoc :: B.Value -> DB.Document
fromDoc (B.Doc document) = document
fromDoc _                = error "*** Database.MongoDB.Wrapper error: extracting Document failed.\n"
