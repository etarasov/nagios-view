{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Main where

import Control.Exception.Control
import Control.Monad.State
import Data.Map hiding (map, (!))
import Data.Maybe
import Data.String
import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Database.HDBC
import Database.HDBC.Sqlite3
import Prelude
import System.Locale (defaultTimeLocale)
import Text.Blaze (ToMarkup)
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Internal
import Text.Blaze.Svg
import qualified Text.Blaze.Svg11 as S
import Text.Blaze.Svg11.Attributes
import Text.Blaze.Svg.Renderer.String

-- PDT: UTC -7 hours
pDT :: TimeZone
pDT = TimeZone { timeZoneMinutes = -(60*7)
               , timeZoneSummerOnly = True
               , timeZoneName = "PDT"
               }

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

year :: Integer
year = 2012

month :: Int
month = 4

day :: Int
day = 15

show_ :: (Show a, Num a, Ord a) => a -> String
show_ a = if a > 9 then show a
                              else "0" ++ show a

dateTime1 :: LocalTime
dateTime1 = LocalTime (fromGregorian year month day) (TimeOfDay 0 0 0)

dateTime2 :: LocalTime
dateTime2 = LocalTime (fromGregorian year month $ day+1) (TimeOfDay 0 0 0)

dateTimeUTC1 :: UTCTime
dateTimeUTC1 = localTimeToUTC pDT dateTime1

dateTimeUTC2 :: UTCTime
dateTimeUTC2 = localTimeToUTC pDT dateTime2

dateTimePosix1 :: Integer
dateTimePosix1 = round $ utcTimeToPOSIXSeconds dateTimeUTC1

dateTimePosix2 :: Integer
dateTimePosix2 = round $ utcTimeToPOSIXSeconds dateTimeUTC2

processAlerts :: [Alert] -> State (Map String [(LocalTime, HostState, String)], Map (String,String) [(LocalTime, ServState, String)]) ()
processAlerts [] = return ()
processAlerts (ServAlert datetime hostname alertattr state statetype checkcount descr : xs) = do
    (hostMap, servMap) <- get
    let servMap' = insertWith (\a b -> b ++ a) (hostname, alertattr) [(utcToLocalTime pDT datetime, state, descr)] servMap
    put (hostMap, servMap')
    processAlerts xs
processAlerts (HostAlert datetime hostname state statetype checkcount descr : xs) = do
    (hostMap, servMap) <- get
    let hostMap' = insertWith (\a b -> b ++ a) hostname [(utcToLocalTime pDT datetime, state, descr)] hostMap
    put (hostMap', servMap)
    processAlerts xs

text_' :: Svg -> Svg
text_' = Parent "text" "<text" "</text>"

diagram :: [((String, String), [(LocalTime, ServState, String)])] -> S.Svg
diagram events =
     S.svg S.! S.customAttribute "xmlns" "http://www.w3.org/2000/svg"
           S.! version "1.1"
           S.! height "600"
           S.! width "1000"
           S.! onload "init(evt)"
           $ do
        S.defs $ do
           S.lineargradient S.! id_ "background"
                            S.! x2 "0"
                            S.! x1 "0"
                            S.! y2 "1"
                            S.! y1 "0"
                            $ do
               S.stop S.! offset "5%"
                      S.! stopColor "#eeeeee"
               S.stop S.! offset "95%"
                      S.! stopColor "#eeeeb0"
        S.rect S.! fill "url(#background)"
               S.! height "600"
               S.! width "1000"
               S.! y "0"
               S.! x "0"

        S.g S.! transform (S.translate 10 10) $ do
            let grid :: Integer -> Svg
                grid a | a > 24 = return ()
                       | otherwise = do
                    let x'= round $ (1000 - 2*10) / 24 * (fromInteger a)
                    S.rect ! fill "#444444"
                           ! y "4"
                           ! x (fromString $ show x')
                           ! height "12"
                           ! width "1"
                    text_' ! fill "#aaaaaa"
                           ! fontFamily "Verdana"
                           ! fontSize "12"
                           ! y "14"
                           ! x ( if a == 24
                                 then
                                    fromString $ show $ x' - 40
                                 else
                                    fromString $ show $ x' + 4
                               )
                           $ if a < 10 then (fromString $ "0"++show a ++":00")
                                       else (fromString $ show a ++ ":00")
                    grid $ a + 3
            grid 0
        diagram' events 0

    where
        diagram' :: [((String, String), [(LocalTime, ServState, String)])] -> Int -> S.Svg
        diagram' [] _ = return ()
        diagram' (event:events) line = do
            S.g S.! transform (S.translate 10 (30 + 20 * line)) $ barLine event
            diagram' events $ line + 1

barLine :: ((String,String), [(LocalTime, ServState, String)]) -> S.Svg
barLine (id_, events) = barLine' (id_, events) Nothing
    where
        barLine' :: ((String, String), [(LocalTime, ServState, String)]) -> Maybe (TimeOfDay, ServState, String) -> S.Svg
        barLine' (_, []) Nothing = return ()
        barLine' ((hostname, alertattr),[]) (Just (startTime, startState, startDescr)) =
            buildBar hostname alertattr startState startDescr startTime $ TimeOfDay 23 59 59
        barLine' ((hostname, alertattr), ((LocalTime _ timeofday, st, descr):alerts)) Nothing = do
            let startTimeState = case st of
                    OK -> Nothing
                    a -> Just (timeofday, a, descr)
            barLine' ((hostname, alertattr), alerts) startTimeState
        barLine' ((hostname, alertattr), ((LocalTime _ timeofday, st, descr):alerts)) (Just (startTime, startState, startDescr)) =
            if st == startState
            then barLine' ((hostname, alertattr), alerts) (Just (startTime, startState, startDescr))
            else do
                let newStartTimeState = case st of
                        OK -> Nothing
                        st -> Just (timeofday, st, descr)
                buildBar hostname alertattr startState startDescr startTime timeofday
                barLine' ((hostname, alertattr), alerts) newStartTimeState

        buildBar :: String -> String -> ServState -> String -> TimeOfDay -> TimeOfDay -> Svg
        buildBar hostname alertattr st descr startTime endTime = do
            let color = case st of
                    CRITICAL -> "#ff0000"
                    WARNING -> "#ffaaaa"
                    UNKNOWN -> "#33bb88"
                    OK -> "#008800"
            S.rect S.! rx "2"
                   S.! ry "2"
                   S.! fill (fromString color)
                   S.! height "16"
                   S.! y "2"
                   S.! x (fromString $ show $ (1000 - 20) * (fromRational $ timeOfDayToDayFraction startTime))
                   S.! width (fromString $ show $ (1000 - 20) * (fromRational $ timeOfDayToDayFraction endTime - timeOfDayToDayFraction startTime))
                   S.! onmouseover (fromString $ "s(evt, '"
                                              ++ hostname
                                              ++ "', '"
                                              ++ alertattr
                                              ++ "', '"
                                              ++ show st
                                              ++ "', '"
                                              ++ eJS descr
                                              ++ "', '"
                                              ++ show startTime
                                              ++ "', '"
                                              ++ show endTime
                                              ++ "', '"
                                              ++ (show $ timeToTimeOfDay $ timeOfDayToTime endTime - timeOfDayToTime startTime)
                                              ++ "')")
                   S.! onmouseout "c()"

eJS :: String -> String
eJS [] = []
eJS ('\n':xs) = " " ++ eJS xs
eJS ('\'':xs) = " " ++ eJS xs
eJS (x:xs) = x : eJS xs

htmlWrapper :: Svg -> Html
htmlWrapper picture = do
    preEscapedToMarkup ("<?xml version=\"1.0\" encoding=\"utf-8\"?>\
 \<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" :: String)
    H.html H.! HA.xmlns "http://www.w3.org/1999/xhtml" $ do
        H.head $ do
            H.title "Nagios analyzer"
            H.meta H.! HA.httpEquiv "Content-Type" H.! HA.content "text/html; charset=utf-8"
            H.link H.! HA.rel "stylesheet" H.! HA.type_ "text/css" H.! HA.href "static/nagios-analyzer.css"
            H.script H.! HA.src "static/nagios-analyzer.js" H.! HA.type_ "text/javascript" $ ""
        H.body $ do
            H.div H.! HA.style "left: 313px; visibility: visible; display: none; top: 521px;" H.! HA.id "detailsPopup" $
                H.ul $ do
                    H.li $ do
                        "Host:"
                        H.span H.! HA.id "host" $ "prod10"
                    H.li $ do
                        "Service:"
                        H.span H.! HA.id "service" $ "epmd status"
                    H.li $ do
                        "State:"
                        H.span H.! HA.id "state" $ "CRITICAL"
                    H.li $ do
                        "Descr:"
                        H.span H.! HA.id "descr" $ "SNMP CRITICAL - *\"epmd: up and running on port 4369 with data:name webhelper at port 52696\" "
                    H.li $ do
                        "Start Time:"
                        H.span H.! HA.id "start" $ "10:31:58"
                    H.li $ do
                        "End Time:"
                        H.span H.! HA.id "end" $ "15:36:58"
                    H.li $ do
                        "Duration:"
                        H.span H.! HA.id "dur" $ "3 hours"
            H.div H.! HA.style "float:left" $
                preEscapedToMarkup $ renderSvg picture

main :: IO ()
main = do
    bracket (connectSqlite3 "nagarchive.db") disconnect $ \conn -> do
        r <- quickQuery' conn "SELECT * from events where datetime > ? and datetime < ? and statetype == \"HARD\" ORDER BY datetime" [toSql dateTimePosix1, toSql dateTimePosix2]
        let convRow :: [SqlValue] -> Alert
            convRow [_,time,type_,host,attr,state,stType,cCount,descr] = case fromSql type_ :: String of
                "SERVICE" -> ServAlert (posixSecondsToUTCTime $ fromSql time)
                                       (fromSql host)
                                       (fromSql attr)
                                       (case fromSql state :: String of
                                            "OK" -> OK
                                            "WARNING" -> WARNING
                                            "CRITICAL" -> CRITICAL
                                            "UNKNOWN" -> UNKNOWN
                                       )
                                       (case fromSql stType :: String of
                                            "SOFT" -> SOFT
                                            "HARD" -> HARD
                                       )
                                       (fromSql cCount)
                                       $ fromSql descr
                "HOST" -> HostAlert (posixSecondsToUTCTime $ fromSql time)
                                    (fromSql host)
                                    (case fromSql state :: String of
                                         "UP" -> UP
                                         "DOWN" -> DOWN
                                    )
                                    (case fromSql stType :: String of
                                         "SOFT" -> SOFT
                                         "HARD" -> HARD
                                    )
                                    (fromSql cCount)
                                    $ fromSql descr

            -- (fromSql a, fromSql b, fromSql c, fromSql d, fromSql e, fromSql f, fromSql g, fromSql h, fromSql i)
        let alerts = map convRow r
        let (hostMap, servMap) = execState (processAlerts alerts) (empty, empty)

        let diagram' = diagram $ assocs servMap
        writeFile "tmp.html" $ renderHtml $ htmlWrapper $ diagram'
