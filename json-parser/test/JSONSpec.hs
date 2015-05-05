module JSONSpec where

import Test.Hspec
import JSON.Type
import Test.QuickCheck
import Data.Map(fromList)

spec :: Spec
spec =
  describe "JSON dumping" $ do
    it "should should be an idempotency" $ property $ \x ->
      (loadJSON.toJSON) x == Right (x :: String)
    it "should should be an idempotency" $ property $ \x ->
      (loadJSON.toJSON) x == Right (x :: Int)
    it "should should be an idempotency" $ property $ \x ->
      (loadJSON.toJSON) x == Right (x :: [Bool])
    it "should should be an idempotency" $ property $ \x ->
      (loadJSON.toJSON) x == Right (x :: Maybe Int)
    it "should should be an idempotency" $ property $ \x ->
      (loadJSON.toJSON $ fromList x) == (Right $ fromList (x:: [(String, String)]))
