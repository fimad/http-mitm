{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
import              Control.Monad
import              Control.Proxy
import              Control.Proxy.Prelude
import              Control.Proxy.TCP           as TCP
import qualified    Data.ByteString             as BS
import              Data.Char
import              Data.Maybe
import              Network.CGI.Protocol (maybeRead)
import              Network.HTTP.Base
import              Network.HTTP.Headers
import              Network.HTTP.MITM.Parse
import              Network.Socket (withSocketsDo)
import              Network.Socket.ByteString
import              Network.URI
import              System.IO

class HttpMessage a where
    toByteString :: a -> BS.ByteString

instance HttpMessage (Request BS.ByteString) where
    toByteString request = BS.pack (map (fromIntegral . fromEnum) $ show request) `BS.append` rqBody request

instance HttpMessage (Response BS.ByteString) where
    toByteString response = BS.pack (map (fromIntegral . fromEnum) $ show response) `BS.append` rspBody response

response400 :: Response BS.ByteString
response400 = Response {
        rspCode     =   (4,0,0)
    ,   rspReason   =   "No host"
    ,   rspHeaders  =   []
    ,   rspBody     =   BS.empty
    }

main :: IO ()
main    =   void
        $   withSocketsDo
        $   serve (Host "127.0.0.1") "1234" 
        $   \(socket,_) 
            ->  runProxy
            $   socketReadS 4096 socket 
            >-> requestD 
            -- >-> showRequestD
            >-> httpProxyD
            >-> mapD toByteString
            >-> takeB 1
            >-> socketWriteD socket 
    where
        showRequestD :: (Proxy p) => () -> Pipe p (Request BS.ByteString) (Request BS.ByteString) IO a
        showRequestD () = runIdentityP $ forever $ do
            r   <-  request ()
            lift $ putStrLn "Got request:"
            lift $ putStr $ show r
            lift $ putStrLn $ show $ rqBody r
            respond r

        httpProxyD :: (Proxy p) => () -> Pipe p (Request BS.ByteString) (Response BS.ByteString) IO a
        httpProxyD () = runIdentityP $ forever $ do
            --lift $ putStrLn "proxy?"
            req             <-  request ()
            --lift $ putStrLn "hello?"
            let port        =   getPort "80" $ rqURI req
            response        <-  lift $ case findHeader HdrHost req of
                Nothing     -> do
                    --putStrLn "uh oh.."
                    return response400
                Just host   ->  do
                    --putStr "connecting to "
                    --putStr host
                    --putStr " : "
                    --putStrLn port
                    connect host port $ \(socket,_) -> do
                            putStrLn "mmmminside the connection!"
                            send socket $ toByteString req
                            putStrLn "inside the connection!"
                            runProxy
                                $   (socketReadS 4096 socket >=> return undefined)
                                >-> responseD
                                >-> request
            --lift $ putStrLn "sending response..."
            --lift $ putStrLn $ map (chr . fromIntegral) $ BS.unpack $ toByteString $ response
            respond response

        -- | Pull a port number from a URI
        getPort :: String -> URI -> String
        getPort defaultPort =   fromMaybe defaultPort
                            .   mfilter (/="")
                            .   liftM (drop 1 . uriPort) -- drop the leading ':'
                            .   uriAuthority 

