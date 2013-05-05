{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.HTTP.MITM (
    HttpMitmConfig(..)
,   defaultConfig
,   runProxy
) where

--import Control.Concurrent
import Control.Concurrent.MonadIO
import Control.Applicative
import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import Data.Word
import Prelude hiding (catch)
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.HTTP.MITM.Parse
import Network.Socket
import Network.URI
import System.IO

-- | Proxy configuration
data HttpMitmConfig = HttpMitmConfig {
        -- | Which port to listen on
        cfgPort         ::  Int
        -- | Which address to bind to
    ,   cfgHost         ::  Word32
        -- | The maximum number of queued connections
    ,   cfgMaxQueued    ::  Int
        -- | The amount of time (in milliseconds) to wait for more data on the
        -- tcp connection before attempting to parse HTTP Requests/Responses
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
    ,   cfgTimeout      =   5000
    ,   cfgMaxHeaderLen =   1024*1024
}

-- | A monad transformer stack for proxies. So far contains the config as a
-- reader wrapping the IO monad.
type Proxy = ReaderT HttpMitmConfig IO

-- | Allow us to fork inside of Proxy
instance HasFork Proxy where
    fork em = do
        r   <-  ask
        lift $ forkIO (runReaderT em r)
    

-- | Run the proxy. This will method will not return.
runProxy :: HttpMitmConfig -> IO ()
runProxy cfg@HttpMitmConfig{..} = do
    sock                <-  socket AF_INET Stream defaultProtocol
    bindSocket sock $ SockAddrInet (fromIntegral cfgPort) 0
    listen sock cfgMaxQueued
    sequence_ $ repeat $ runReaderT (acceptAndFork sock) cfg
    where
        acceptAndFork :: Socket -> Proxy ()
        acceptAndFork sock = do
            (client,_)  <-  lift $ accept sock
            fork $ connectionThread client
            return ()

-- | Handle's a single connection.
connectionThread :: Socket -> Proxy ()
connectionThread sock = do
    handle              <-  lift $ socketToHandle sock ReadWriteMode
    headerChars         <-  hGetHttpHeader handle
    let eitherRequest   =   headerChars >>= parseRequest
    -- Gets a string encoding of the response, and a closure which will close
    -- the host handle if one was opened.
    (response,closer)   <-  case eitherRequest >>= liftM2 (,) <$> (return . id) <*> getHost of
        -- We could not process the request, so we will reply that we cannot
        Left reason     ->  --do
            return (return $ show Response {
                rspCode     =   (4,0,0)
            ,   rspReason   =   reason
            ,   rspHeaders  =   []
            ,   rspBody     =   ""
            }, return ())
        -- We can process the request, forward it and get the server's response
        Right (request,host)    ->  do
            -- | Look up the host and connect to it
            hostInfo    <-  lift $ getAddrInfo (Just defaultHints) (Just host) Nothing
            hostSock    <-  lift $ socket AF_INET Stream defaultProtocol
            lift $ connect hostSock $ addrAddress $ head hostInfo
            hostHandle  <-  lift $ socketToHandle hostSock ReadWriteMode
            -- | Pass on the request to the host
            requestBody <-  hGetContentsAvailable handle
            lift $ print request
            lift $ hPutStr hostHandle $ show request
            lift $ hPutStr hostHandle requestBody
            -- | Get the response from the host
            replyHeader <-  hGetHttpHeader hostHandle
            replyBody   <-  hGetContentsAvailable hostHandle
            return (liftM2 (++) replyHeader (return replyBody), hClose hostHandle)
    lift $ hPutStr handle $ either id id response
    lift $ hClose handle
    lift closer
    where
        noHostError = "No host specified"
        getHost Request{..} =
            case uriRegName <$> uriAuthority rqURI of
                Just host   ->  return host
                Nothing     ->  fromMaybe (Left noHostError)
                            $   Right <$> lookupHeader HdrHost rqHeaders


-- | Receives a request, modifies the request and sends it to the host specified
-- in the modified request, it then relays the response back to the client.
--handleRequest :: Handle -> Proxy ()
--handleRequest handle = do
    

-- | Read the raw string required for the Header portion of an HTTP message.
hGetHttpHeader :: Handle -> Proxy (Either HttpError String)
hGetHttpHeader handle = do
    HttpMitmConfig{..}  <-  ask
    getHelper cfgMaxHeaderLen ""
    where
        getHelper :: Int -> String -> Proxy (Either HttpError String)
        getHelper 0 !last4  =   return $! Left "Header is too long"
        getHelper !i !last4 =   do
            maybeChar   <-  hGetCharMaybe handle
            case maybeChar of
                Nothing     ->  return $! Left "Connection timeout..."
                Just c      -> do
                    let newLast4 = take 4 $! c:last4
                    case newLast4 of
                        -- stored in reverse order
                        "\n\r\n\r"  ->  return $ Right [c]
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
    hasContent          <-  lift
                        $   catch (hWaitForInput handle cfgTimeout)
                            (\(_ :: IOException) -> return False)
    if hasContent
        then do
            c   <-  lift $ hGetChar handle
            return $! Just c
        else
            return Nothing

