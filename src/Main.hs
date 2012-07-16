
module Main where

import Control.Concurrent (forkIO, killThread)
import Control.Monad.Error
import Data.Monoid
import Happstack.Server.FileServe
import Happstack.Server.Internal.Monads
import Happstack.Server.SimpleHTTP
import NagView.DailyDiagram as DailyDiagram
import Network.Socket
import System.Posix.Syslog

main :: IO ()
main = do
    let httpConf = Conf 8080 Nothing Nothing 60
    sock <- socket AF_INET Stream defaultProtocol
    setSocketOption sock ReuseAddr 1
    loopbackIp <- inet_addr "127.0.0.1"
    bindSocket sock $
        SockAddrInet 8080 loopbackIp
    listen sock (max 1024 maxListenQueue)

    httpTid <- forkIO $ simpleHTTPWithSocket' unpackErrorT
                                              sock
                                              httpConf
                                              $ decodeBody (defaultBodyPolicy "/tmp/" 4096 20000 40000 )
                                              >> control
    waitForTermination
    syslog Notice "Shutting down..."
    killThread httpTid
    syslog Notice "Shutdown complete"

unpackErrorT :: (Monad m) => UnWebT (ErrorT String m) a -> UnWebT m a
unpackErrorT handler = do
    resE <- runErrorT handler
    case resE of
        Left err -> return $ Just (Left $ toResponse err, Set mempty)
        Right x -> return x

control :: ServerPartT (ErrorT String IO) Response
control = msum [ nullDir >> seeOther "/mainpage" (toResponse "")
               , dir "mainpage" $ return $ toResponse "Hello" -- MainPage.pageHandler
               , dir "dailydiagram" DailyDiagram.pageHandler
               , dir "static" $ serveDirectory EnableBrowsing [] "static"
               ]
