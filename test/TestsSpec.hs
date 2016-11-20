-----------------------------------------------------------------------------
--
-- Module      :  TestsSpec
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module TestsSpec (spec) where
import Prototypemain
import Test.Hspec

spec :: Spec
spec = do
    describe "encode" $ do
        it "encodes multiples of 3" $
            encode "no-padding!!" `shouldBe` "bm8tcGFkZGluZyEh"
        it "encodes multiples of 3 + 1" $
            encode "padding  2" `shouldBe` "cGFkZGluZyAgMg=="
        it "encodes multiples of 3 + 2" $
            encode "padding1" `shouldBe` "cGFkZGluZzE="




