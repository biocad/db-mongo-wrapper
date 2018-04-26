{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.MongoDB.Wrapper.Internal.InteractionNew
  (
    FromJsonConfig (..)
  , mongoContextFromJsonConfig
  , encode
  , decode
  , find
  , findAll
  , accessM
  ) where

import           Control.Monad.IO.Class                               (MonadIO,
                                                                       liftIO)
import           Data.Aeson                                           (FromJSON (..),
                                                                       ToJSON (..),
                                                                       fromJSON,
                                                                       toJSON)
import           Data.Aeson.Types                                     (Result (..))
import qualified Data.Bson                                            as B (Value (..))
import           Data.Map.Strict                                      (findWithDefault)
import           Data.Text                                            (Text,
                                                                       pack)
import qualified Database.MongoDB                                     as MDB
import           Database.MongoDB.Wrapper.Internal.AesonBsonConverter (fromDocument,
                                                                       toBson)
import           System.BCD.Config.Mongo                              (FromJsonConfig (..),
                                                                       MongoConfig (..))

-- | Loads 'Host' from config.json.
--
instance FromJsonConfig MDB.Host where
  fromJsonConfig = do
      MongoConfig{..} <- fromJsonConfig
      pure . MDB.readHostPort $ _host ++ ":" ++ show _port

-- | Loads 'Pipe' from config.json.
--
instance FromJsonConfig MDB.Pipe where
  fromJsonConfig = fromJsonConfig >>= liftIO . MDB.connect


-- | Loads 'MongoContext' from config.json.
--
mongoContextFromJsonConfig :: String -> IO MDB.MongoContext
mongoContextFromJsonConfig databaseVar = do
    MongoConfig{..} <- fromJsonConfig
    pipe'           <- fromJsonConfig
    let databaseName = findWithDefault (error $ "Mongo database could not be found by var: " ++ databaseVar) databaseVar _databases
    _ <- testAccess pipe' databaseName (pack _user) (pack _password)
    pure $ MDB.MongoContext pipe' MDB.master databaseName
  where
    testAccess :: MDB.Pipe -> MDB.Database -> MDB.Username -> MDB.Password -> IO ()
    testAccess pipe database username password = MDB.access pipe MDB.UnconfirmedWrites database (MDB.auth username password) >> pure ()

-- | For given 'MongoContext' and 'Action' runs it and returns result from database.
--
accessM :: MonadIO m => MDB.MongoContext -> MDB.Action m a -> m a
accessM (MDB.MongoContext pipe access database) = MDB.access pipe access database

-- | Encodes from object to 'Document'.
--
encode :: ToJSON a => a -> MDB.Document
encode = fromDoc . toBson . toJSON

-- | Decodes from 'Document' to either object.
--
decode :: FromJSON a => MDB.Document -> Either String a
decode = fromResult . fromJSON . fromDocument

-- | Redefine find function from Database.MongoDB.
--
find :: MonadIO m => Int -> Text -> [MDB.Field] -> MDB.Action m [MDB.Document]
find count collection selection = MDB.find ((MDB.select selection collection) {MDB.limit = fromIntegral count}) >>= MDB.rest

-- | Find for given collection and selectors.
--
findAll :: MonadIO m => Text -> [MDB.Field] -> MDB.Action m [MDB.Document]
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
fromDoc :: B.Value -> MDB.Document
fromDoc (B.Doc document) = document
fromDoc _                = error "*** Database.MongoDB.Wrapper error: extracting Document failed.\n"
