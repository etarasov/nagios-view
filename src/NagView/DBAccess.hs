
module NagView.DBAccess where

import Control.Exception.Lifted (bracket)
import Control.Monad.Error
import Data.Time
import Data.Time.Clock.POSIX
import Database.HDBC
import Database.HDBC.Sqlite3
import NagView.Types

naConnect :: IO Connection
naConnect = connectSqlite3 "nagarchive.db"

getAlerts :: UTCTime -> UTCTime -> ErrorT String IO [Alert]
getAlerts begin end = do
    let dateTimePosix1 = round $ utcTimeToPOSIXSeconds begin :: Integer
    let dateTimePosix2 = round $ utcTimeToPOSIXSeconds end :: Integer
    bracket (liftIO naConnect)
            (liftIO . disconnect)
            $ \ conn -> do
        r <- liftIO $ quickQuery' conn "SELECT * from events where datetime > ? and datetime < ? and statetype == \"HARD\" ORDER BY datetime" [toSql dateTimePosix1, toSql dateTimePosix2]
        mapM convRow r
    where
        convRow :: [SqlValue] -> ErrorT String IO Alert
        convRow [] = throwError "Error while getting alert list 1"
        convRow [_,time,type_,host,attr,state,stType,cCount,descr] = case fromSql type_ :: String of
            "SERVICE" -> do
                state' <- case fromSql state :: String of
                    "OK" -> return OK
                    "WARNING" -> return WARNING
                    "CRITICAL" -> return CRITICAL
                    "UNKNOWN" -> return UNKNOWN
                    a -> throwError $ "Unknown alert status: " ++ a
                stType' <- case fromSql stType :: String of
                    "SOFT" -> return SOFT
                    "HARD" -> return HARD
                    a -> throwError $ "Unknown alert type: " ++ a
                return $ ServAlert (posixSecondsToUTCTime $ fromSql time)
                                   (fromSql host)
                                   (fromSql attr)
                                   state'
                                   stType'
                                   (fromSql cCount)
                                   $ fromSql descr
            "HOST" -> do
                state' <- case fromSql state :: String of
                    "UP" -> return UP
                    "DOWN" -> return DOWN
                    a -> throwError $ "Unknown alert status: " ++ a
                stType' <- case fromSql stType :: String of
                    "SOFT" -> return SOFT
                    "HARD" -> return HARD
                    a -> throwError $ "Unknown alert type: " ++ a

                return $ HostAlert (posixSecondsToUTCTime $ fromSql time)
                                   (fromSql host)
                                   state'
                                   stType'
                                   (fromSql cCount)
                                   $ fromSql descr
            a -> throwError $ "Unknown service type: " ++ a
        convRow _ = throwError "Error while getting alert list 2"
