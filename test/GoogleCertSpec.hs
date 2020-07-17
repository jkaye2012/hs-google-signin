{-# LANGUAGE OverloadedStrings #-}

module GoogleCertSpec (spec) where

import qualified Data.Map                as Map
import qualified Data.Text               as T
import qualified Data.Time               as Time
import qualified Data.Time.Calendar      as Cal
import           Test.Hspec
import qualified Web.Google.Certificates as C

spec :: Spec
spec = do
  describe "mkGoogleCerts" $ do

    context "with well-formed time input" $ do
      it "creates a certificate record" $ do
        let pem = Map.fromList [("someguid", "somecert")]
        let cert = C.mkGoogleCerts pem "Wed, 15 Jul 2020 09:09:57 GMT"
        cert `shouldBe` Right (C.GoogleCerts pem (Time.UTCTime (Cal.fromGregorian 2020 7 15) 32997))

    context "with ill-formed time input" $ do
      it "returns a malformed expiry error" $ do
        let pem = Map.fromList [("someguid", "somecert")]
        let cert = C.mkGoogleCerts pem "asdf this is a bad timestamp"
        cert `shouldBe` Left (C.MalformedExpiry "asdf this is a bad timestamp")

  describe "parseResponse" $ do
    context "with successful response" $ do
      it "creates certificates" $ do
        pending
    context "with json exception" $ do
      it "returns a parsing error" $ do
        pending
    context "without expected headers" $ do
      it "returns a missing expiry error" $ do
        let resp = C.parseResponse 200 (Right $ Map.fromList []) []
        resp `shouldBe` Left C.MissingExpiry
    context "with non-200 return code" $ do
      it "returns a network error" $ do
        let resp = C.parseResponse 500 (Right $ Map.fromList []) []
        resp `shouldBe` Left (C.NetworkError 500)
