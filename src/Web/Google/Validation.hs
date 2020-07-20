{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Validation () where

import qualified Data.Text               as T
import qualified Web.Google.Certificates as Certs

-- Next time: use a decoded certificate and a supplied id token to verify a signature

data IdToken

-- verifyIdToken :: Certs.GoogleCerts -> T.Text -> Maybe IdToken
-- verifyIdToken = _
