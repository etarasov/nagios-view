
module NagView.Utils where

import Control.Monad.Error
import Control.Monad.State
import Data.Map
import Data.Time.LocalTime
import Happstack.Server
import NagView.Types
import Safe

-- PDT: UTC -7 hours
pDT :: TimeZone
pDT = TimeZone { timeZoneMinutes = -(60*7)
               , timeZoneSummerOnly = True
               , timeZoneName = "PDT"
               }

arrangeAlertsByAttr :: [Alert]
                    -> ( Map String [(LocalTime, HostState, String)]  -- Host Events
                       , Map (String,String) [(LocalTime, ServState, String)]) -- Service Events
arrangeAlertsByAttr alerts = execState (arrangeAlertsByAttr' alerts) (empty, empty)

arrangeAlertsByAttr' :: [Alert]
                     -> State ( Map String [(LocalTime, HostState, String)]  -- Host Events
                              , Map (String,String) [(LocalTime, ServState, String)]) () -- Service Events
arrangeAlertsByAttr' [] = return ()
arrangeAlertsByAttr' (ServAlert datetime hostname alertattr state statetype checkcount descr : xs) = do
    (hostMap, servMap) <- get
    let servMap' = insertWith (\a b -> b ++ a) (hostname, alertattr) [(utcToLocalTime pDT datetime, state, descr)] servMap
    put (hostMap, servMap')
    arrangeAlertsByAttr' xs
arrangeAlertsByAttr' (HostAlert datetime hostname state statetype checkcount descr : xs) = do
    (hostMap, servMap) <- get
    let hostMap' = insertWith (\a b -> b ++ a) hostname [(utcToLocalTime pDT datetime, state, descr)] hostMap
    put (hostMap', servMap)
    arrangeAlertsByAttr' xs

getInputString :: MonadIO m => String -> ServerPartT (ErrorT String m) String
getInputString input = do
    resE <- getDataFn $ look input
    case resE of
        Left _ -> throwError $ "Parameter not found: " ++ input
        Right res -> return res

getInputNonEmptyString :: MonadIO m => String -> ServerPartT (ErrorT String m) String
getInputNonEmptyString input = do
    res <- getInputString input
    if res == ""
        then throwError $ "Parameter should not be empty: " ++ input
        else return res

getInputOrEmptyString :: MonadIO m => String -> ServerPartT (ErrorT String m) String
getInputOrEmptyString input = do
    b <- isThereInput input
    if not b
        then
            return ""
        else
            getInputString input

getInputRead :: (Read a, MonadIO m) => String -> ServerPartT (ErrorT String m) a
getInputRead input = do
    resS <- getInputString input
    let resM = readMay resS
    case resM of
        Just res -> return res
        Nothing -> throwError $ "Error while parsing parameter: " ++ input

isThereInput :: MonadIO m => String -> ServerPartT (ErrorT String m) Bool
isThereInput input = do
    resE <- getDataFn $ look input
    return $ case resE of
                Left _ -> False
                Right _ -> True
