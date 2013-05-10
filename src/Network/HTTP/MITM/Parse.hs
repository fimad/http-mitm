{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.MITM.Parse (
    requestD
,   responseD
,   parseRequest
,   parseResponse
) where

import              Control.Applicative ((<$>), (<*), (*>), (<*>), (<|>))
import              Control.Monad
import              Control.Proxy
import              Control.Proxy.Attoparsec
import              Data.Char
import              Data.Maybe
import              Data.Word
import qualified    Data.ByteString             as BS
import              Network.CGI.Protocol (maybeRead)
import              Network.HTTP.Base
import              Network.HTTP.Headers
import              Network.URI
--import Data.Attoparsec
import              Data.Attoparsec.ByteString  as AP
import              Data.Attoparsec.Combinator



--------------------------------------------------------------------------------
-- Exposed API
--------------------------------------------------------------------------------

-- | A `Pipe` that requests `ByteString`s and responds `Request`s.
requestD :: (Monad m, Proxy p) => () -> Pipe p BS.ByteString (Request BS.ByteString) m r
requestD    = parserInputD >-> parserD parseRequest
 
-- | A `Pipe` that requests `ByteString`s and responds `Response`s.
responseD :: (Monad m, Proxy p) => () -> Pipe p BS.ByteString (Response BS.ByteString) m r
responseD   = parserInputD >-> parserD parseResponse


--------------------------------------------------------------------------------
-- Make porting from Parsec easier
--------------------------------------------------------------------------------

char :: Char -> Parser Word8
char = satisfy . (==) . fromIntegral . ord

char' :: Char -> Parser Char
char' = fmap (chr . fromIntegral) . char

notChar :: Char -> Parser Word8
notChar c = satisfy $ not . (==(fromIntegral $ ord c))
--notChar = satisfy . (curry $ (not . (uncurry $ (==) . fromIntegral . ord)))

notChar' :: Char -> Parser Char
notChar' = fmap (chr . fromIntegral) .  notChar

anyChar :: Parser Word8
anyChar = satisfy (return True)

anyChar' :: Parser Char
anyChar' = fmap (chr . fromIntegral) $ anyChar

digit :: Parser Word8
digit = satisfy isDigit
    where isDigit w = w >= 48 && w <= 57

many = many'

--noneOf = 

--------------------------------------------------------------------------------
-- Attoparsec parser definitions
--------------------------------------------------------------------------------

newlineP :: Parser ()
newlineP = void $ try (char '\r' >> char '\n')

spacesP :: Parser ()
spacesP = void $ many1 (char ' ')

-- | Parse an HTTP request.
parseResponse :: Parser (Response BS.ByteString)
parseResponse = do
    status  <-  statusP
    reason  <-  reasonP
    headers <-  headersP
    body    <-  bodyP headers
    return Response {
        rspCode     =   status
    ,   rspReason   =   reason
    ,   rspHeaders  =   headers
    ,   rspBody     =   body
    }

-- | Parse an HTTP request.
parseRequest :: Parser (Request BS.ByteString)
parseRequest = do
    method  <-  methodP
    uri     <-  uriP
    headers <-  headersP
    body    <-  bodyP headers
    return Request {
        rqURI       =   uri
    ,   rqMethod    =   method
    ,   rqHeaders   =   headers
    ,   rqBody      =   body
    }

-- | Skips the http version and returns the status code.
statusP :: Parser ResponseCode
statusP =   manyTill anyChar spacesP
        *>  ((,,) <$> num <*> num <*> num)
        <*  spacesP
    where
        num :: Parser Int
        num = read . return . chr . fromIntegral <$> digit

reasonP :: Parser String
reasonP = manyTill (chr . fromIntegral<$> anyChar) newlineP

-- | Parse the method, consuming the trailing space
methodP :: Parser RequestMethod
methodP =   (helper "HEAD"                      >>  return HEAD)
        <|> (helper "PUT"                       >>  return PUT )
        <|> (helper "GET"                       >>  return GET)
        <|> (helper "POST"                      >>  return POST)
        <|> (helper "DELETE"                    >>  return DELETE)
        <|> (helper "OPTIONS"                   >>  return OPTIONS)
        <|> (helper "TRACE"                     >>  return TRACE)
        <|> (helper "CONNECT"                   >>  return CONNECT)
        <|> (many (notChar' ' ') <* spacesP     >>= return . Custom)
    where
        helper value = string value <* spacesP

-- | Parse the URI resource, and consume till the end of the line
uriP :: Parser URI
uriP    =   failIfNothing
        =<< parseURIReference
        <$> many (notChar' ' ')
        <*  spacesP
        <*  manyTill anyChar newlineP -- parse till the end of line
    where
        failIfNothing (Just v)  = return v
        failIfNothing _         = fail "Bad URI"

-- | Parses a list of headers followed by a blank line
headersP :: Parser [Header]
headersP = manyTill headerP newlineP

-- | Parse a single header.
headerP :: Parser Header
headerP = mkHeader <$> headerNameP <*> headerValueP

-- | Parse the value of a header, consuming the newline.
headerValueP :: Parser String
headerValueP = manyTill anyChar' newlineP

-- | Parses a header name, consuming the colon and leading white space
headerNameP :: Parser HeaderName
headerNameP =   (helper "Cache-Control"             >>  return HdrCacheControl)
            <|> (helper "Connection"                >>  return HdrConnection)
            <|> (helper "Date"                      >>  return HdrDate)
            <|> (helper "Pragma"                    >>  return HdrPragma)
            <|> (helper "Transfer-Encoding"         >>  return HdrTransferEncoding)
            <|> (helper "Upgrade"                   >>  return HdrUpgrade)
            <|> (helper "Via"                       >>  return HdrVia)
            <|> (helper "Accept"                    >>  return HdrAccept)
            <|> (helper "Accept-Charset"            >>  return HdrAcceptCharset)
            <|> (helper "Accept-Encoding"           >>  return HdrAcceptEncoding)
            <|> (helper "Accept-Language"           >>  return HdrAcceptLanguage)
            <|> (helper "Authorization"             >>  return HdrAuthorization)
            <|> (helper "Cookie"                    >>  return HdrCookie)
            <|> (helper "Expect"                    >>  return HdrExpect)
            <|> (helper "From"                      >>  return HdrFrom)
            <|> (helper "Host"                      >>  return HdrHost)
            <|> (helper "If-Modified-Since"         >>  return HdrIfModifiedSince)
            <|> (helper "If-Match"                  >>  return HdrIfMatch)
            <|> (helper "If-None-Match"             >>  return HdrIfNoneMatch)
            <|> (helper "If-Range"                  >>  return HdrIfRange)
            <|> (helper "If-Unmodified-Since"       >>  return HdrIfUnmodifiedSince)
            <|> (helper "Max-Forwards"              >>  return HdrMaxForwards)
            <|> (helper "Proxy-Authorization"       >>  return HdrProxyAuthorization)
            <|> (helper "Range"                     >>  return HdrRange)
            <|> (helper "Referer"                   >>  return HdrReferer)
            <|> (helper "User-Agent"                >>  return HdrUserAgent)
            <|> (helper "Age"                       >>  return HdrAge)
            <|> (helper "Location"                  >>  return HdrLocation)
            <|> (helper "Proxy-Authenticate"        >>  return HdrProxyAuthenticate)
            <|> (helper "Public"                    >>  return HdrPublic)
            <|> (helper "Retry-After"               >>  return HdrRetryAfter)
            <|> (helper "Server"                    >>  return HdrServer)
            <|> (helper "Set-Cookie"                >>  return HdrSetCookie)
            <|> (helper "TE"                        >>  return HdrTE)
            <|> (helper "Trailer"                   >>  return HdrTrailer)
            <|> (helper "Vary"                      >>  return HdrVary)
            <|> (helper "Warning"                   >>  return HdrWarning)
            <|> (helper "WWW-Authenticate"          >>  return HdrWWWAuthenticate)
            <|> (helper "Allow"                     >>  return HdrAllow)
            <|> (helper "Content-Base"              >>  return HdrContentBase)
            <|> (helper "Content-Encoding"          >>  return HdrContentEncoding)
            <|> (helper "Content-Language"          >>  return HdrContentLanguage)
            <|> (helper "Content-Length"            >>  return HdrContentLength)
            <|> (helper "Content-Location"          >>  return HdrContentLocation)
            <|> (helper "Content-MD5"               >>  return HdrContentMD5)
            <|> (helper "Content-Range"             >>  return HdrContentRange)
            <|> (helper "Content-Type"              >>  return HdrContentType)
            <|> (helper "ETag"                      >>  return HdrETag)
            <|> (helper "Expires"                   >>  return HdrExpires)
            <|> (helper "Last-Modified"             >>  return HdrLastModified)
            <|> (helper "Content-Transfer-Encoding" >>  return HdrContentTransferEncoding)
            <|> (helperParser (many $ fmap (chr . fromIntegral) $ (satisfy $ (&&) <$> (/=32) <*> (/=58)))  >>= return . HdrCustom)
    where
        helper header = helperParser (try (string header))
        helperParser parser = parser <* char ':' <* spacesP

bodyP :: [Header] -> Parser BS.ByteString
bodyP headers   =   fromMaybe (return BS.empty)
                $   (lookupHeader HdrContentLength headers >>= maybeRead >>= return . parseFixedLength)
                <|> (lookupHeader HdrTransferEncoding headers >> return parseChunked)
    where
        parseFixedLength :: Int -> Parser BS.ByteString
        parseFixedLength length = AP.take length

        parseChunked :: Parser BS.ByteString
        parseChunked = return BS.empty

