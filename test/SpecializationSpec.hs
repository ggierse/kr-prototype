module SpecializationSpec (spec) where
import Prototype.Basis
import Prototype.Specialization
import Test.Hspec

import TestData

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "changeExpressionIsSpecialization" $ do
        context "one change expression is a specialization of another if" $ do
          it "they are equal" $
            changeExpressionIsSpecializationOf changeNameMyName changeNameMyName `shouldBe` True
          it "the specialized is a subset of the generalization" $
            changeExpressionIsSpecializationOf changeWheelsToTwo changeWheelsTwoFour `shouldBe` True
          it "the specialized fullfills a number constraint exposed by the generalization" $
            True `shouldBe` False
          it "the specialized fullfills an existance quantification constraint exposed by the generalization" $
            True `shouldBe` False
        context "one change expression is not a specialization of another if" $
          it "they describe a differently named property" $
            changeExpressionIsSpecializationOf changeNameMyName changeWheelsMyName `shouldBe` False
    describe "isSpecialization" $
      context "one PrototypeExpression is a specialization of another if" $ do
        it "all properties are a changeExpresionSpecialization" $
          True `shouldBe` False
        it "should be false if PrototypeExpressions are not fixpoints" $
          True `shouldBe` False
