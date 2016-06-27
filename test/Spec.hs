import Test.Hspec

import Data.Vector as V
import qualified Op
import qualified Tape
import qualified State

checkOp op checks =
  let tape = Tape.new
      state = State.new
      n = State.stepop op tape state
  in checks state n


main :: IO ()
main = hspec $ do
  describe "Prelude.head" $
    it "returns the first element of a list" $
      Prelude.head [23 ..] `shouldBe` (23 :: Int)

  describe "Ops" $ do
    it "should increment data" $
      checkOp Op.Add (\s -> \n -> do
          State.ip n `shouldBe` 1
          State.mem n ! 0 `shouldBe` 1)

    it "should decrement data" $
      checkOp Op.Sub (\s -> \n -> do
          State.ip n `shouldBe` 1
          State.mem n ! 0 `shouldBe` 255)

    it "should increment data pointer" $
      checkOp Op.PRight (\s -> \n -> do
          State.ip n `shouldBe` 1
          State.dp n `shouldBe` 1
          State.mem n ! 0 `shouldBe` 0)

    it "should increment data pointer" $
      checkOp Op.PLeft (\s -> \n -> do
          State.ip n `shouldBe` 1
          State.dp n `shouldBe` (V.length . State.mem $ n) - 1
          State.mem n ! 0 `shouldBe` 0)


