{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Char as Char
import Lens.Family2 ((^.), (%~), (.~))
import qualified Lens.Family2.TH as LFTH

import Test.Hspec (hspec, describe, it, shouldBe)

data Pair a b = Pair { _pairL :: a, _pairR :: b }
              deriving (Eq, Show, Read, Ord)
$(LFTH.makeLenses ''Pair)

p :: Pair Char Int
p = Pair '1' 1

main = hspec $ do
  describe "makeLenses" $ do
    it "makes lenses that function with lens-family operators" $ do
      (p ^. pairL) `shouldBe` '1'
      (p ^. pairR) `shouldBe` (1 :: Int)
      ((pairL %~ Char.digitToInt) p) `shouldBe` (Pair 1 1 :: Pair Int Int)
      ((pairR %~ Char.intToDigit) p) `shouldBe` Pair '1' '1'
      ((pairL .~ "foo") p) `shouldBe` Pair "foo" (1 :: Int)
      ((pairR .~ "bar") p) `shouldBe` Pair '1' "bar"
