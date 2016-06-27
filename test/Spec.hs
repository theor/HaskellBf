import Test.Hspec

import qualified Data.Word
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Op
import qualified Tape
import qualified State

checkOp op checks =
  let tape = Tape.new
      state = State.new
      n = State.stepop op tape state
  in checks state n

checkOpPreds op preds =
  checkOp op (\ s n ->
    mapM_ (\x -> x n) preds)

shouldEq x y n = x n `shouldBe` y

shouldEqF x y n = x n `shouldBe` y n

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $
    it "returns the first element of a list" $
      head [23 ..] `shouldBe` (23 :: Int)

  describe "Ops" $ do
    it "should increment data too" $
      checkOpPreds Op.Add
        [ State.ip `shouldEq` 1
        , State.memAt 0 `shouldEq` (1 :: Data.Word.Word8) ]

    it "should decrement data" $
      checkOpPreds Op.Sub
        [ State.ip `shouldEq` 1
        , State.memAt 0 `shouldEq` 255 ]

    it "should increment data pointer" $
      checkOpPreds Op.PRight
          [ State.ip `shouldEq` 1
          , State.dp `shouldEq` 1
          , State.memAt 0 `shouldEq` 0 ]

    it "should increment data pointer" $
      checkOpPreds Op.PLeft
          [ State.ip `shouldEq` 1
          , State.dp `shouldEqF` (flip (-) 1 . V.length . State.mem)
          , State.memAt 0 `shouldEq` 0 ]
