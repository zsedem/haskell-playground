module PreludeSpec where

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec =
    describe "composition" $ do
        let composed = (+10).(*2)
            simple x = (x * 2) + 10
        it "should always return the same" $
          property $ \x -> composed x == simple (x::Int)


