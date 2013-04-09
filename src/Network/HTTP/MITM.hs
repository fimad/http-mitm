{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Network.MITM.HTTP (
    HttpMitmConfig(..)
,   defaultProtocol
,   runProxy
) where

--import Control.Concurrent
import Control.Concurrent.MonadIO
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Data.Word
import Data.List
import Network.HTTP.MITM.Parse
import Network.Socket
import System.IO

-- | Proxy configuration
data HttpMitmConfig = HttpMitmConfig {
        -- | Which port to listen on
        cfgPort         ::  Int
        -- | Which address to bind to
    ,   cfgHost         ::  Word32
        -- | The maximum number of queued connections
    ,   cfgMaxQueued    ::  Int
        -- | The amount of time to wait for more data on the tcp connection
        -- before attempting to parse HTTP Requests/Responses
    ,   cfgTimeout      ::  Int
        -- | The maximum size in bytes that the proxy will allow for the HTTP
        -- message discounting the message body.
    ,   cfgMaxHeaderLen ::  Int
} deriving (Show,Eq)

-- | Sane defaults. Port is set to 4747 and host is set to 0.0.0.0
defaultConfig :: HttpMitmConfig
defaultConfig = HttpMitmConfig {
        cfgPort         =   4747
    ,   cfgHost         =   0
    ,   cfgMaxQueued    =   5
    ,   cfgTimeout      =   1000
    ,   cfgMaxHeaderLen =   1024*1024
}

-- | A monad transformer stack for proxies. So far contains the config as a
-- reader wrapping the IO monad.
type Proxy = ReaderT HttpMitmConfig IO

-- | Allow us to fork inside of Proxy
--instance HasFork (ReaderT r0 IO) where
instance HasFork Proxy where
    fork em = do
        r   <-  ask
        lift $ forkIO (runReaderT em r)
    

-- | Run the proxy. This will method will not return.
runProxy :: Proxy ()
runProxy = do
    HttpMitmConfig{..}  <-  ask
    sock                <-  lift $ socket AF_INET Stream defaultProtocol
    lift $ bindSocket sock $ SockAddrInet (fromIntegral cfgPort) 0
    lift $ listen sock cfgMaxQueued
    sequence_ $ repeat $ acceptAndFork sock
    where
        acceptAndFork :: Socket -> Proxy ()
        acceptAndFork sock = do
            (client,_)  <-  lift $ accept sock
            fork $ connectionThread client
            return ()

-- | Handle's a single connection.
connectionThread :: Socket -> Proxy ()
connectionThread sock = do
    handle      <-  lift $ socketToHandle sock ReadWriteMode
    headerChars <-  hGetHttpHeader handle
    return ()

-- | Read the raw string required for the Header portion of an HTTP message.
hGetHttpHeader :: Handle -> Proxy (Maybe String)
hGetHttpHeader handle = do
    HttpMitmConfig{..}  <-  ask
    getHelper cfgMaxHeaderLen ""
    where
        getHelper :: Int -> String -> Proxy (Maybe String)
        getHelper 0 !last4  =   return Nothing
        getHelper !i !last4 =   do
            maybeChar   <-  hGetCharMaybe handle
            case maybeChar of
                Nothing     ->  return Nothing
                Just c      -> do
                    let newLast4 = take 4 $! c:last4
                    case newLast4 of
                        -- stored in reverse order
                        "\n\r\n\r"  ->  return $ Just [c]
                        _           ->  getHelper (i-1) newLast4
                                    >>= \cs -> return $! (c:) <$> cs

-- | Read until we get a timeout
hGetContentsAvailable :: Handle -> Proxy String
hGetContentsAvailable handle = do
    HttpMitmConfig{..}  <-  ask
    c                   <-  hGetCharMaybe handle
    case c of 
        Just c  ->  do
            cs  <-  hGetContentsAvailable handle
            return $! c:cs
        Nothing ->  return []

-- | Read a char off the wire if one is available.
hGetCharMaybe :: Handle -> Proxy (Maybe Char)
hGetCharMaybe handle = do
    HttpMitmConfig{..}  <-  ask
    hasContent          <-  lift $ hWaitForInput handle cfgTimeout
    if hasContent
        then do
            c   <-  lift $ hGetChar handle
            return $! Just c
        else
            return Nothing

