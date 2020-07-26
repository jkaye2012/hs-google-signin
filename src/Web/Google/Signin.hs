module Web.Google.Signin
  (
    -- * Entrypoints
    initialState
  , signin
    -- * Data types
  , V.UnverifiedToken
  , V.VerifiedToken
  , SigninState
  , SigninResult(..)
  , SigninT
  -- * Error types
  , C.CertificateError(..)
  , V.VerificationError(..)
  , SigninError(..)
  )
  where

import           Control.Monad.State
import           Data.Either.Combinators (mapLeft)
import qualified Data.Time               as Time
import qualified Web.Google.Certificates as C
import qualified Web.Google.Validation   as V

-- | Errors that can result from a signin identification request.
data SigninError =
  -- | Failure to download or parse Google's HMAC certificates.
  -- Without valid certificates, identity verification cannot be performed.
  -- This is most likely to be a temporary technical error that will usually resolve itself.
  CertificateError C.CertificateError
  -- | Failure to verify an identity token against a valid certificate.
  -- Generally, this error indicates an issue with the end user.
  | VerificationError V.VerificationError
  deriving (Eq, Show)

-- | Internal state for signin requests.
-- Google rotates the certificates required for identity validation regularly;
-- 'SigninState' holds cached certificates as long as they are valid.
data SigninState =
  NewSigninState
  | HasCerts C.GoogleCerts
  | HasError C.CertificateError
  deriving (Eq, Show)

type StateResult = Either C.CertificateError C.PemCerts

-- | The result of a single identification request.
type SigninResult = Either SigninError V.VerifiedToken

-- | The MonadState transformer in which identity verification is run.
type SigninT m = StateT SigninState m

runSigninIO :: (MonadIO m) => SigninState -> m (StateResult, SigninState)
runSigninIO s = case s of
  NewSigninState -> tryDownloadCerts
  (HasError _)   -> tryDownloadCerts
  (HasCerts c)   -> returnOrRefreshCerts c
  where
    tryDownloadCerts = do
      c <- C.googleCertificates
      return $ case c of
        Left err    -> (Left err, HasError err)
        Right certs -> (Right $ C.pemCerts certs, HasCerts certs)
    returnOrRefreshCerts c@(C.GoogleCerts pem exp) = do
      now <- liftIO Time.getCurrentTime
      if now < exp
        then return (Right pem, HasCerts c)
        else tryDownloadCerts

-- | Performs a single identification request.
-- The identification token should be received from an external Google API.
signin :: (MonadIO m) => V.UnverifiedToken -> SigninT m SigninResult
signin t = do
  s <- get
  (a, s) <- runSigninIO s
  put s
  return $ mapLeft CertificateError a >>= mapLeft VerificationError . flip V.verifyIdToken t

-- | The starting 'SigninState' that can be used to run the 'SigninT' monad stack.
initialState :: SigninState
initialState = NewSigninState
