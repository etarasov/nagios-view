
module NagView.Types where

import Data.Time

data HostState = UP | DOWN
                 deriving (Show)
data ServState = OK | WARNING | CRITICAL | UNKNOWN
                 deriving (Show, Eq)
data StateType = SOFT | HARD
                 deriving (Show)

data Alert = ServAlert { dateTime :: UTCTime
                       , hostName :: String
                       , alertAttr :: String
                       , sState :: ServState
                       , sStateType :: StateType
                       , sCheckCount :: Integer
                       , sDescr :: String
                       }
           | HostAlert { dateTime :: UTCTime
                       , hostName :: String
                       , hState :: HostState
                       , hStateType :: StateType
                       , hCheckCount :: Integer
                       , hDescr :: String
                       } deriving (Show)

