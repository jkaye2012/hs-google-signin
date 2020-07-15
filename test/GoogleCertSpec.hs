{-# LANGUAGE OverloadedStrings #-}

module GoogleCertSpec (spec) where

import qualified Data.Map                as Map
import qualified Data.Text               as T
import qualified Data.Time               as Time
import           Data.Time.Calendar
import           Test.Hspec
import qualified Web.Google.Certificates as C

spec :: Spec
spec = do
  describe "mkGoogleCerts" $ do

    context "with well-formed time input" $ do
      it "creates a certificate record" $ do
        let pem = Map.fromList [("someguid", "somecert")]
        let cert = C.mkGoogleCerts pem "Wed, 15 Jul 2020 09:09:57 GMT"
        cert `shouldBe` Right (C.GoogleCerts pem (Time.UTCTime (fromGregorian 2020 7 15) 32997))

    context "with ill-formed time input" $ do
      it "returns a malformed expiry error" $ do
        let pem = Map.fromList [("someguid", "somecert")]
        let cert = C.mkGoogleCerts pem "asdf this is a bad timestamp"
        cert `shouldBe` Left (C.MalformedExpiry "asdf this is a bad timestamp")
