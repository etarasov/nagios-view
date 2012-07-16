
module NagView.Utils where

import Control.Monad.State
import Data.Map
import Data.Time.LocalTime
import NagView.Types

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

