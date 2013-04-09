module Network.HTTP.MITM.Parse (
    HttpError
,   parseRequest
,   parseResponse
) where

import Control.Applicative ((<$>), (<*), (*>), (<*>))
import Control.Monad
import Data.Maybe
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.URI
import Text.Parsec
import Text.Parsec.String


--------------------------------------------------------------------------------
-- Exposed API
--------------------------------------------------------------------------------

-- A String that contains the error and the status code. The idea is that an
-- Http Response can easily be created from this to send back to the client.
type HttpError = String

parseHelper :: Parser a -> String -> Either HttpError a
parseHelper parser contents = case parse parser "" contents of
    Left error      ->  Left $! show error
    Right result    ->  Right result

-- | Parse the header portion of an HTTP 'Request'.
parseRequest :: String -> Either HttpError (Request ())
parseRequest = parseHelper requestP

-- | Parse the header portion of an HTTP 'Response'.
parseResponse :: String -> Either HttpError (Response ())
parseResponse = parseHelper responseP


--------------------------------------------------------------------------------
-- Parsec parser definitions
--------------------------------------------------------------------------------

newlineP :: Parser ()
newlineP = try (string "\r\n") >> return ()

spacesP :: Parser ()
spacesP = many1 (char ' ') >> return ()

-- | Parse the request header, ignoring the body.
responseP :: Parser (Response ())
responseP = do
    status  <-  statusP
    reason  <-  reasonP
    headers <-  headersP
    return $! Response {
        rspCode     =   status
    ,   rspReason   =   reason
    ,   rspHeaders  =   headers
    ,   rspBody     =   ()
    }

-- | Parse the request header, ignoring the body.
requestP :: Parser (Request ())
requestP = do
    method  <-  methodP
    uri     <-  uriP
    headers <-  headersP
    return $! Request {
        rqURI       =   uri
    ,   rqMethod    =   method
    ,   rqHeaders   =   headers
    ,   rqBody      =   ()
    }

-- | Skips the http version and returns the status code.
statusP :: Parser ResponseCode
statusP =   manyTill anyChar spacesP
        *>  ((,,) <$> num <*> num <*> num)
        <*  spacesP
    where
        num :: Parser Int
        num = read . return <$> digit

reasonP :: Parser String
reasonP = manyTill anyChar newlineP

-- | Parse the method, consuming the trailing space
methodP :: Parser RequestMethod
methodP =   (helper "HEAD"                  >>  return HEAD)
        <|> (helper "PUT"                   >>  return PUT )
        <|> (helper "GET"                   >>  return GET)
        <|> (helper "POST"                  >>  return POST)
        <|> (helper "DELETE"                >>  return DELETE)
        <|> (helper "OPTIONS"               >>  return OPTIONS)
        <|> (helper "TRACE"                 >>  return TRACE)
        <|> (helper "CONNECT"               >>  return CONNECT)
        <|> (many (noneOf " ") <* spacesP   >>= return . Custom)
    where
        helper value = string value <* spacesP

-- | Parse the URI resource, and consume till the end of the line
uriP :: Parser URI
uriP    =   failIfNothing
        =<< parseURI
        <$> many (noneOf " ")
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
headerValueP = manyTill anyChar newlineP

-- | Parses a header name, consuming the colon and leading white space
headerNameP :: Parser HeaderName
headerNameP =   (helper "Cache-Control"              >> return HdrCacheControl)
            <|> (helper "Connection"                 >> return HdrConnection)
            <|> (helper "Date"                       >> return HdrDate)
            <|> (helper "Pragma"                     >> return HdrPragma)
            <|> (helper "Transfer-Encoding"          >> return HdrTransferEncoding)
            <|> (helper "Upgrade"                    >> return HdrUpgrade)
            <|> (helper "Via"                        >> return HdrVia)
            <|> (helper "Accept"                     >> return HdrAccept)
            <|> (helper "Accept-Charset"             >> return HdrAcceptCharset)
            <|> (helper "Accept-Encoding"            >> return HdrAcceptEncoding)
            <|> (helper "Accept-Language"            >> return HdrAcceptLanguage)
            <|> (helper "Authorization"              >> return HdrAuthorization)
            <|> (helper "Cookie"                     >> return HdrCookie)
            <|> (helper "Expect"                     >> return HdrExpect)
            <|> (helper "From"                       >> return HdrFrom)
            <|> (helper "Host"                       >> return HdrHost)
            <|> (helper "If-Modified-Since"          >> return HdrIfModifiedSince)
            <|> (helper "If-Match"                   >> return HdrIfMatch)
            <|> (helper "If-None-Match"              >> return HdrIfNoneMatch)
            <|> (helper "If-Range"                   >> return HdrIfRange)
            <|> (helper "If-Unmodified-Since"        >> return HdrIfUnmodifiedSince)
            <|> (helper "Max-Forwards"               >> return HdrMaxForwards)
            <|> (helper "Proxy-Authorization"        >> return HdrProxyAuthorization)
            <|> (helper "Range"                      >> return HdrRange)
            <|> (helper "Referer"                    >> return HdrReferer)
            <|> (helper "User-Agent"                 >> return HdrUserAgent)
            <|> (helper "Age"                        >> return HdrAge)
            <|> (helper "Location"                   >> return HdrLocation)
            <|> (helper "Proxy-Authenticate"         >> return HdrProxyAuthenticate)
            <|> (helper "Public"                     >> return HdrPublic)
            <|> (helper "Retry-After"                >> return HdrRetryAfter)
            <|> (helper "Server"                     >> return HdrServer)
            <|> (helper "Set-Cookie"                 >> return HdrSetCookie)
            <|> (helper "TE"                         >> return HdrTE)
            <|> (helper "Trailer"                    >> return HdrTrailer)
            <|> (helper "Vary"                       >> return HdrVary)
            <|> (helper "Warning"                    >> return HdrWarning)
            <|> (helper "WWW-Authenticate"           >> return HdrWWWAuthenticate)
            <|> (helper "Allow"                      >> return HdrAllow)
            <|> (helper "Content-Base"               >> return HdrContentBase)
            <|> (helper "Content-Encoding"           >> return HdrContentEncoding)
            <|> (helper "Content-Language"           >> return HdrContentLanguage)
            <|> (helper "Content-Length"             >> return HdrContentLength)
            <|> (helper "Content-Location"           >> return HdrContentLocation)
            <|> (helper "Content-MD5"                >> return HdrContentMD5)
            <|> (helper "Content-Range"              >> return HdrContentRange)
            <|> (helper "Content-Type"               >> return HdrContentType)
            <|> (helper "ETag"                       >> return HdrETag)
            <|> (helper "Expires"                    >> return HdrExpires)
            <|> (helper "Last-Modified"              >> return HdrLastModified)
            <|> (helper "Content-Transfer-Encoding"  >> return HdrContentTransferEncoding)
            <|> (helperParser (many (noneOf " :"))   >>= return . HdrCustom)
    where
        helper header = helperParser (try (string header))
        helperParser parser = parser <* char ':' <* spacesP
