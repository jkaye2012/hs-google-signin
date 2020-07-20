{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Web.Google.Certificates
  (
    googleCertificates
  , RawCerts
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
import qualified Web.JWT                   as JWT

deriving instance Show JWT.Signer
deriving instance Eq JWT.Signer

type RawCerts = Map.Map T.Text T.Text
type PemCerts = Map.Map T.Text JWT.Signer
type Expiry = Time.UTCTime
data GoogleCerts = GoogleCerts PemCerts Expiry
                   deriving (Show, Eq)

data CertificateError = NetworkError Int
                      | ParsingError HTTP.JSONException
                      | MalformedCerts
                      | MissingExpiry
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


maybeToRight :: a -> Maybe b -> Either a b
maybeToRight d Nothing  = Left d
maybeToRight _ (Just v) = Right v

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
  return (parseResponse' resp)
