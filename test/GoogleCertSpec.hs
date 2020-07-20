{-# LANGUAGE OverloadedStrings #-}

module GoogleCertSpec (spec) where

import qualified Data.Map                as Map
import qualified Data.Text               as T
import qualified Data.Time               as Time
import qualified Data.Time.Calendar      as Cal
import           Test.Hspec
import qualified Web.Google.Certificates as C
import qualified Web.JWT                 as JWT

cert = "-----BEGIN CERTIFICATE-----\nMIIDJjCCAg6gAwIBAgIIeIT+4f7qIO8wDQYJKoZIhvcNAQEFBQAwNjE0MDIGA1UE\nAxMrZmVkZXJhdGVkLXNpZ25vbi5zeXN0ZW0uZ3NlcnZpY2VhY2NvdW50LmNvbTAe\nFw0yMDA3MDMwNDI5MzVaFw0yMDA3MTkxNjQ0MzVaMDYxNDAyBgNVBAMTK2ZlZGVy\nYXRlZC1zaWdub24uc3lzdGVtLmdzZXJ2aWNlYWNjb3VudC5jb20wggEiMA0GCSqG\nSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDnvj0IBvUtquJnRii58pEdbTxVlQNLUqoL\nfmNsGtOB9xZ8+v7NVsp3G0tgy1n2bePHFwan4L4/ULHtDfy0KJyPUaJhJN7s2bSi\nKgk9HGfdHjxeBGjvyLnJg5heCklI8gRYBPpsFKqQflNgfgZZyho67itFvfrzKCyF\nOpyPJD/rhiDZmnYJ7UickjNJ/CVxrITneeDYYli/taa+7r9gauK7uhVWvjVN46i9\nm9ypxqRMI8TWdEw68Tm/K/ute/lLHOyTe2bJEEbQsFmpEu8zhaEi9+8EXYJPduuV\n826rHersxgqdrmfzFGXckrwO5yy1cZeRX32eXVbzX869vFxPbrxjAgMBAAGjODA2\nMAwGA1UdEwEB/wQCMAAwDgYDVR0PAQH/BAQDAgeAMBYGA1UdJQEB/wQMMAoGCCsG\nAQUFBwMCMA0GCSqGSIb3DQEBBQUAA4IBAQCNzZ/eNmKxL2LQprmRdBJ1AWOV2VC6\n/pWeYkO9WNMD2f82CaQ1EaIVul/laj4EFxSP0TUwK5FpA1v7ss6+SOxCPVR5aBeW\nxw6VqyQU/1XNnE3vwl+jBWHy2/ksnxa6QQLMCjDO9KEFt/Xga/GXaBTvMCSnkPD+\nk5eNIIbRb8MopT1Z5CgkCbmRh91pGeoGxHk46ASrIRpdGZOZejrD6gUyQkZYMj6/\nL2qEyLiSnlbRnWEy9fyRtrd7W5H99VawzODP7rdjEMZsxykPR6ajM03uCgGyq8e8\nh03MJnrMsenV0MSJaoe79cl3deK1I4z2mlQYsvqCnzHbV/YutU7EJ9HQ\n-----END CERTIFICATE-----"

spec :: Spec
spec = do
  describe "mkGoogleCerts" $ do
    context "with well-formed time input" $ do
      it "creates a certificate record" $ do
        let raw = Map.fromList [("someguid", cert)] :: C.RawCerts
        let c = C.mkGoogleCerts raw "Wed, 15 Jul 2020 09:09:57 GMT"
        let signer = JWT.hmacSecret cert
        let pem = Map.fromList [("someguid", signer)]
        c `shouldBe` Right (C.GoogleCerts pem (Time.UTCTime (Cal.fromGregorian 2020 7 15) 32997))

    context "with ill-formed time input" $ do
      it "returns a malformed expiry error" $ do
        let pem = Map.fromList [("someguid", cert)]
        let cert = C.mkGoogleCerts pem "asdf this is a bad timestamp"
        cert `shouldBe` Left (C.MalformedExpiry "asdf this is a bad timestamp")

  describe "parseResponse" $ do
    context "with successful response" $ do
      it "creates certificates" $ do
        let pem = Map.fromList [("someguid", cert)]
        let resp = C.parseResponse 200 (Right pem) ["Wed, 15 Jul 2020 09:09:57 GMT"]
        let cert = C.mkGoogleCerts pem "Wed, 15 Jul 2020 09:09:57 GMT"
        resp `shouldBe` cert

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
