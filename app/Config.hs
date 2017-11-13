module Config(
    Config(..)
  , ServerConfig(..)
) where

import Data.Data
import Data.Time
import Data.Text (Text)
import GHC.Generics

import System.ConfigApp

data Config = Config {
    appServer   :: ServerConfig
  , appDuration :: NominalDiffTime
  , appRebootCmd :: Text
  , appLastRebootTimeFilePath :: Maybe ConfigPath
  , appDebugMode :: Maybe Bool
} deriving (Show, Eq, Generic, Data)

data ServerConfig = ServerConfig {
    serverHost :: Text
  , serverPort :: Int
} deriving (Show, Eq, Generic, Data)

$(deriveFromJSON defaultOptions ''ServerConfig)
$(deriveFromJSON defaultOptions ''Config)
