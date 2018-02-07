{-# LANGUAGE OverloadedStrings #-}

module Database.MongoDB.Wrapper.Internal.Types
  ( Config (..)
  , DeployConfig (..)
  , MongoConfig (..)
  ) where

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))

data Config
  = Config { deploy :: DeployConfig
           } deriving (Eq, Show)

instance FromJSON Config where
 parseJSON = withObject "Config" $ \o -> Config <$>
                                         o .: "deploy"

instance ToJSON Config where
 toJSON p = object [ "deploy" .= deploy p
                   ]

data DeployConfig
  = DeployConfig { mongo :: MongoConfig
                 } deriving (Eq, Show)

instance FromJSON DeployConfig where
  parseJSON = withObject "DeployConfig" $ \o -> DeployConfig <$>
                                                o .: "mongo"

instance ToJSON DeployConfig where
  toJSON p = object [ "mongo" .= mongo p
                    ]

data MongoConfig
  = MongoConfig { cHost     :: String
                } deriving (Eq, Show)

instance FromJSON MongoConfig where
  parseJSON = withObject "MongoConfig" $ \o -> MongoConfig <$>
                                               o .: "host"

instance ToJSON MongoConfig where
  toJSON p = object [ "host" .= cHost p
                    ]
