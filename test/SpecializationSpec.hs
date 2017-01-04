module SpecializationSpec (spec) where
import Prototype.Specialization
import Test.Hspec
import qualified Prototype.Basis as Basis

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
    describe "isSpecializationOf" $ do
      it "a Constraint is a specialization of another Constraint if the numbers fit" $
        Const (Atleast 5) `isSpecializationOf` Const (Atleast 3)
      it "an empty set is a specialization of an empty set" $
        (Set.empty :: Set.Set Basis.IRI) `isSpecializationOf` (Set.empty :: Set.Set Basis.IRI)
      it "if only constraints are used the definition for iris does not hinder the implementation" $ -- TODO: this test is to much dependant on the implementation
        getIris (Set.singleton (Const (Atleast 5))) `isSpecializationOf` getIris (Set.singleton (Const (Atleast 3)))
      it "a set of constraints is a specialization of a constraint" $
        Set.singleton (Atleast 5)  `isSpecializationOf` Atleast 3
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
            it "mixed instances and constraints in the generalization" $
              (threeChildren `isSpecializationOf` constPlusChild) `shouldBe` True
            it "bigger at least" $
              (getChangeExpression hasChildren [Const (Atleast 5)]
                `isSpecializationOf`
                getChangeExpression hasChildren [Const (Atleast 4)])
              `shouldBe` True
            describe "combined constraints and instances" $ do
              it "atleast constraint alone should make it specialization" $
                (mixedAtLeast3Iri1 `isSpecializationOf` childLeast2Constraint) `shouldBe` True
              it "atmost is equal and one filler" $
                (mixedAtMost2Iri1 `isSpecializationOf` childAtmost2Constraint) `shouldBe` True
              it "atmost is equal and maximal fillers" $
                (mixedAtMost2Iri2 `isSpecializationOf` childAtmost2Constraint) `shouldBe` True



          --it "the specialized fullfills a type restriction constraint" $
          --  True `shouldBe` False
          --it "the specialized fullfills an existance quantification constraint exposed by the generalization" $
          --  True `shouldBe` False
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
            it "exactly constraint cannot be specialized by constraint" $
              (childExactly1Constraint `isSpecializationOf` childExactly2Constraint) `shouldBe` False



    describe "isSpecialization" $
      context "one PrototypeDefinition is a specialization of another if" $ do
        it "all properties of the general are a changeExpressionSpecialization" $
          (carWithComputerProto `isSpecializationOf` computerProtoG) `shouldBe` True
      --  it "should be false if PrototypeDefinitions are not fixpoints" $
        --  True `shouldBe` False
