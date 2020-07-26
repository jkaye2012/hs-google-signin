{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Web.Google.Certificates
  (
    -- * Entrypoint
    googleCertificates
    -- * Data types
  , RawCerts
  , PemCerts
  , Expiry
  , GoogleCerts(..)
    -- * Utility functions
  , mkGoogleCerts
  , parseResponse
  , pemCerts
  , expiry
  -- * Error types
  , CertificateError(..)
  ) where

import           Control.Monad.IO.Class    (MonadIO, liftIO)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C
import           Data.Either.Combinators   (maybeToRight)
import qualified Data.Map                  as Map
import qualified Data.Text                 as T
import qualified Data.Time                 as Time
import qualified Network.HTTP.Simple       as HTTP
import qualified Network.HTTP.Types.Header as Header
import qualified Web.JWT                   as JWT

deriving instance Show JWT.Signer
deriving instance Eq JWT.Signer

-- | Raw certificates downloaded from Google' certificate servers.
type RawCerts = Map.Map T.Text T.Text
-- | Parsed HMAC certificates that can be used for identity verification.
type PemCerts = Map.Map T.Text JWT.Signer
-- | Tracks the expiration time of a set of 'PemCerts'
type Expiry = Time.UTCTime
-- | Fully parsed and validated certificates.
data GoogleCerts = GoogleCerts PemCerts Expiry
                   deriving (Show, Eq)

-- | Extract the 'PemCerts' from a set of certificates.
pemCerts :: GoogleCerts -> PemCerts
pemCerts (GoogleCerts pem _) = pem

-- | Extract the 'Expiry' from a set of certificates.
expiry :: GoogleCerts -> Expiry
expiry (GoogleCerts _ exp) = exp

-- | Errors that can occur while download or parsing certificates from Google's servers.
data CertificateError =
  -- | A connection could not be established to the certificate servers.
  -- The contained 'Int' is the HTTP response code for the failed request.
  NetworkError Int
  -- | Certificates were successfully downloaded, but could not be parsed as json.
  | ParsingError HTTP.JSONException
  -- | Certificates were successfully downloaded and parsed, but did not contain the expected payload.
  | MalformedCerts
  -- | Certificates were successfully downloaded and parsed, but did not contain an expiration header.
  | MissingExpiry
  -- | Certificates were successfully downloaded and parsed, but the time in the expiration header could not be interpreted.
  -- The contained 'String' is the raw value of the malformed expiration header.
  | MalformedExpiry String
  deriving (Show)

instance Eq CertificateError where
  NetworkError l == NetworkError r = l == r
  ParsingError l == ParsingError r = show l == show r
  MalformedExpiry l == MalformedExpiry r = l == r
  MissingExpiry == MissingExpiry = True
  _ == _ = False

parseRawCerts :: RawCerts -> PemCerts
parseRawCerts = fmap JWT.hmacSecret

mkGoogleCerts :: RawCerts -> String -> Either CertificateError GoogleCerts
mkGoogleCerts c e = do
  exp <- parsedExp
  Right $ GoogleCerts parsedCerts exp
  where
    parsedCerts = parseRawCerts c
    parsedExp = maybeToRight (MalformedExpiry e) $ Time.parseTimeM True Time.defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" e

parseResponse :: Int -> (Either HTTP.JSONException RawCerts) -> [String] -> Either CertificateError GoogleCerts
parseResponse code body headers =
    case (code, body, headers) of
      (200, Right certs, exp : []) -> mkGoogleCerts certs exp
      (200, Left exc, _)           -> Left (ParsingError exc)
      (200, _, _)                  -> Left MissingExpiry
      (status, _, _)               -> Left (NetworkError status)

parseResponse' :: HTTP.Response (Either HTTP.JSONException RawCerts) -> Either CertificateError GoogleCerts
parseResponse' resp =
    parseResponse (HTTP.getResponseStatusCode resp) (HTTP.getResponseBody resp) $ fmap C.unpack (HTTP.getResponseHeader Header.hExpires resp)

rawCertificates :: (MonadIO m) => m (HTTP.Response (Either HTTP.JSONException RawCerts))
rawCertificates = HTTP.httpJSONEither "https://www.googleapis.com/oauth2/v1/certs"

googleCertificates :: (MonadIO m) => m (Either CertificateError GoogleCerts)
googleCertificates = do
  resp <- rawCertificates
  return $ parseResponse' resp
