{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module Web.Google.Signin
  (
    V.UnverifiedToken
  , V.VerifiedToken
  , initialState
  , SigninResult(..)
  , SigninT
  , MonadSignin(..)
  , tokenVerifier
  , C.CertificateError(..)
  , V.VerificationError(..)
  , SigninError(..)
  )
  where

import           Control.Monad.State
import           Data.Either.Combinators (mapLeft)
import           Data.IORef
import qualified Data.Text               as T
import qualified Data.Time               as Time
import qualified Web.Google.Certificates as C
import qualified Web.Google.Validation   as V

data SigninError = CertificateError C.CertificateError
                  | VerificationError V.VerificationError
                  deriving (Eq, Show)

data SigninState = NewSigninState
                 | HasCerts C.GoogleCerts
                 | HasError C.CertificateError
                 deriving (Eq, Show)

initialState :: SigninState
initialState = NewSigninState

type StateResult = Either C.CertificateError C.PemCerts

type SigninResult = Either SigninError V.VerifiedToken

type SigninT m = StateT SigninState m

runSigninIO :: SigninState -> StateT SigninState IO (StateResult, SigninState)
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

signinState :: SigninT IO StateResult
signinState = do
  s <- get
  (a, s) <- runSigninIO s
  put s
  return a

class (Monad m) => MonadSignin m where
  verifyToken :: SigninState -> V.UnverifiedToken -> m (SigninResult, SigninState)

instance MonadSignin IO where
  verifyToken state t = do
    (a, s) <- runStateT signinState state
    return $ (mapLeft CertificateError a >>= mapLeft VerificationError . flip V.verifyIdToken t, s)

tokenVerifier :: IO (V.UnverifiedToken -> IO SigninResult)
tokenVerifier = do
  ref <- newIORef initialState
  return $ \t -> do
    s <- readIORef ref
    (a, s) <- verifyToken s t
    writeIORef ref s
    return a
