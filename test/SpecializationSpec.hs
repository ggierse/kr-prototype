module SpecializationSpec (spec) where
import Prototype.Specialization
import Test.Hspec

import TestData

-- import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "iris and constraints" $ do
      it "a set of iris is a specialization of an atleast constraint" $
        Set.fromList [jan, susan] `isSpecializationOf` Atleast 1 `shouldBe` True
      it "a set of iris is a specialization of an atmost constraint" $
        Set.fromList [jan, susan] `isSpecializationOf` Atmost 5 `shouldBe` True
      it "a set of iris is a specialization of an exactly constraint" $
        Set.fromList [jan, susan] `isSpecializationOf` Exactly 2 `shouldBe` True
    describe "changeExpressionIsSpecialization" $ do
        context "one change expression is a specialization of another if" $ do
          it "they are equal" $
            isSpecializationOf changeNameMyName changeNameMyName `shouldBe` True
          it "the specialized is a subset of the generalization" $
            isSpecializationOf changeWheelsToTwo changeWheelsTwoFour `shouldBe` True
          context "the specialized fullfills a number constraint exposed by the generalization" $ do
            it "at least constraint" $
              (threeChildren `isSpecializationOf` childLeast2Constraint) `shouldBe` True
            it "at most constraint" $
              (oneChild `isSpecializationOf` childAtmost2Constraint) `shouldBe` True
            it "exactly constraint" $
              (oneChild `isSpecializationOf` childExactly1Constraint) `shouldBe` True


          it "the specialized fullfills an existance quantification constraint exposed by the generalization" $
            True `shouldBe` False
        context "one change expression is not a specialization of another if" $ do
          it "they describe a differently named property" $
            isSpecializationOf changeNameMyName changeWheelsMyName `shouldBe` False
          context "the number constraint is not fullfilled" $ do
            it "at least constraint" $
               oneChild `isSpecializationOf` childLeast2Constraint `shouldBe` False
            it "at most constraint" $
              (threeChildren `isSpecializationOf`  childAtmost2Constraint) `shouldBe` False
            it "exactly constraint" $
              (threeChildren `isSpecializationOf`  childExactly1Constraint) `shouldBe` False

    describe "isSpecialization" $
      context "one PrototypeDefinition is a specialization of another if" $ do
        it "all properties are a changeExpressionSpecialization" $
          True `shouldBe` False
        it "should be false if PrototypeDefinitions are not fixpoints" $
          True `shouldBe` False
