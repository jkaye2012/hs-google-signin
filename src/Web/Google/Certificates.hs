{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Web.Google.Certificates
  (
    googleCertificates
  , PemCerts
  , Expiry
  , GoogleCerts(..)
  , CertificateError(..)
  , mkGoogleCerts
  , parseResponse
  ) where

import           Control.Monad.IO.Class    (MonadIO)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as C
import qualified Data.Map                  as Map
import qualified Data.Text                 as T
import qualified Data.Time                 as Time
import qualified Network.HTTP.Simple       as HTTP
import qualified Network.HTTP.Types.Header as Header

type PemCerts = Map.Map T.Text T.Text
type Expiry = Time.UTCTime
data GoogleCerts = GoogleCerts PemCerts Expiry
                   deriving (Show, Eq)

data CertificateError = NetworkError Int
                      | ParsingError HTTP.JSONException
                      | MissingExpiry
                      | MalformedExpiry String
                      deriving (Show)

instance Eq CertificateError where
  NetworkError l == NetworkError r = l == r
  ParsingError l == ParsingError r = show l == show r
  MalformedExpiry l == MalformedExpiry r = l == r
  MissingExpiry == MissingExpiry = True
  _ == _ = False

mkGoogleCerts :: PemCerts -> String -> Either CertificateError GoogleCerts
mkGoogleCerts pem exp =
  case parsed of
    Just t  -> Right (GoogleCerts pem t)
    Nothing -> Left (MalformedExpiry exp)
  where
    parsed = Time.parseTimeM True Time.defaultTimeLocale "%a, %d %b %Y %H:%M:%S %Z" exp

parseResponse :: HTTP.Response (Either HTTP.JSONException PemCerts) -> Either CertificateError GoogleCerts
parseResponse resp =
    case (HTTP.getResponseStatusCode resp, HTTP.getResponseBody resp, HTTP.getResponseHeader Header.hExpires resp) of
      (200, Right certs, exp : []) -> mkGoogleCerts certs (C.unpack exp)
      (200, Left exc, _)           -> Left (ParsingError exc)
      (200, _, _)                  -> Left MissingExpiry
      (status, _, _)               -> Left (NetworkError status)

rawCertificates :: (MonadIO m) => m (HTTP.Response (Either HTTP.JSONException PemCerts))
rawCertificates = HTTP.httpJSONEither (HTTP.parseRequest_ "https://www.googleapis.com/oauth2/v1/certs")

googleCertificates :: (MonadIO m) => m (Either CertificateError GoogleCerts)
googleCertificates = do
  resp <- rawCertificates
  return (parseResponse resp)
