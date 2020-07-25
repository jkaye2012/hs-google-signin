{-# LANGUAGE OverloadedStrings #-}

module Web.Google.Validation
  (
    UnverifiedToken
  , VerifiedToken
  , VerificationError(..)
  , verifyIdToken
  ) where

import           Data.Either.Combinators (maybeToRight)
import qualified Data.Map                as Map
import qualified Data.Text               as T
import qualified Web.Google.Certificates as Certs
import qualified Web.JWT                 as JWT

type UnverifiedToken = T.Text
type VerifiedToken = JWT.JWT JWT.VerifiedJWT

data VerificationError = InvalidIdToken T.Text
                       | MalformedIdToken
                       | MissingCertificate T.Text
                       | VerificationFailed
                       deriving (Show, Eq)

verifyIdToken :: Certs.PemCerts -> UnverifiedToken -> Either VerificationError VerifiedToken
verifyIdToken certs token = do
  u <- maybeToRight (InvalidIdToken token) $ JWT.decode token
  k <- maybeToRight MalformedIdToken . JWT.kid . JWT.header $ u
  s <- maybeToRight (MissingCertificate k) $ Map.lookup k certs
  maybeToRight VerificationFailed $ JWT.verify s u
