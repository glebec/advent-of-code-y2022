module LibSpec where

import Test.Hspec
import Lib (greeting)

spec :: Spec
spec = do
    describe "greeting" $ do
        it "is \"Hello\"" $ do
            greeting `shouldBe` "Hello"
