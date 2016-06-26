import Test.Hspec

import Lib

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ do
      head [23 ..] `shouldBe` (23 :: Int)

  describe "Ops" $ do
    it "should increment data" $ do
      stepop Add
