module SpecializationSpec (spec) where
import Prototype.Basis
import Prototype.Specialization
import Test.Hspec

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "changeExpressionIsSpecialization" $ do
        context "one change expression is a specialization of another if" $ do
          it "they are equal" $
            True `shouldBe` False
          it "the specialized is a subset of the generalization" $
            True `shouldBe` False
          it "the specialized fullfills a number constraint exposed by the generalization" $
            True `shouldBe` False
    describe "isSpecialization" $
      context "one PrototypeExpression is a specialization of another if" $
        it "all properties are a changeExpresionSpecialization" $
          True `shouldBe` False
