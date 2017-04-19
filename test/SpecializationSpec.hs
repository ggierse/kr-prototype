module SpecializationSpec (spec) where
import Prototype.Specialization
import Test.Hspec
import qualified Prototype.Basis as Basis

import TestData
import ComposedPrototypesData
import SpecializationData
--import SpecializationOldData

-- import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "isSatisfied allValuesFrom" $ do
      it "a set of iris that is equal to the constraint set is satisfied" $
        isSatisfied threeNamesSet (generateAllConstraint threeNamesSet)
          `shouldBe` True
      it "a set of iris that is a subset of the constraint is satisfied" $
        isSatisfied twoNamesSet (generateAllConstraint threeNamesSet)
          `shouldBe` True
      it "a set with one iri not occuring in the constraint is not satisfied" $
        isSatisfied threeNamesSet (generateAllConstraint twoNamesSet)
          `shouldBe` False
    describe "isSatisfied someValuesFrom" $ do
      it "a set of iris that is equal to the constraint set is satisfied" $
        isSatisfied threeNamesSet (generateSomeConstraint threeNamesSet)
          `shouldBe` True
      it "a set of iris that contains one element of the constraint is satisfied" $
        isSatisfied (Set.singleton jan) (generateSomeConstraint threeNamesSet)
          `shouldBe` True
      it "the set of iris may contain iris not element of the constraint" $
        isSatisfied  threeNamesSet (generateSomeConstraint (Set.singleton jan))
          `shouldBe` True
      it "a set with no iri occuring in the constraint is not satisfied" $
        isSatisfied (Set.fromList [tamara, frank])
              (generateSomeConstraint twoNamesSet)
          `shouldBe` False
      it "an empty set with non empty constraint is not satisfied" $
        isSatisfied Set.empty (generateSomeConstraint twoNamesSet)
          `shouldBe` False
      it "an empty set with empty constraint is not satisfied" $
        isSatisfied Set.empty (generateSomeConstraint Set.empty)
          `shouldBe` False
    describe "isSatisfied Cardinality Constraints" $ do
      it "a set of iris satisfies an atleast constraint" $
        isSatisfied twoNamesSet (generateCardConstraintLower 1)
          `shouldBe` True
      it "a set of iris satisfies an atmost constraint" $
        isSatisfied twoNamesSet (generateCardConstraintUpper 5)
          `shouldBe` True
      it "a set of iris satisfies an exactly constraint" $
        isSatisfied twoNamesSet (generateCardConstraint 2 2)
          `shouldBe` True
      it "a larger set of iris that the upper bound does not satisfy" $
        isSatisfied threeNamesSet (generateCardConstraintUpper 1)
          `shouldBe` False
      it "a smaller set of iris that the lower bound does not satisfy" $
        isSatisfied twoNamesSet (generateCardConstraintLower 3)
          `shouldBe` False
    describe "isMatched" $ do
      --it "a Constraint is a specialization of another Constraint if the numbers fit" $
      --  Const (Atleast 5) `isSpecializationOf` Const (Atleast 3)
      it "matching a cardinality constraint lower bound" $
        isMatched (Set.singleton (generateCardConstraintLower 5)) (generateCardConstraintLower 3)
          `shouldBe` True
      it "matching a cardinality constraint upper bound" $
        isMatched (Set.singleton (generateCardConstraintUpper 5)) (generateCardConstraintUpper 6)
          `shouldBe` True
      it "matching a cardinality constraint bounded both ways" $
        isMatched (Set.singleton (generateCardConstraint 2 5)) (generateCardConstraint 1 6)
          `shouldBe` True
      -- TODO generate more test cases for matching
      {--
    describe "isPropertySpecialization" $ do
        context "one change expression is a specialization of another if" $ do
          it "they are equal" $
            isPropertySpecialization changeNameMyName changeNameMyName `shouldBe` True
          it "the specialized is a superset of the generalization" $
            isPropertySpecialization changeWheelsTwoFour changeWheelsToTwo `shouldBe` True
          context "the specialized fullfills a number constraint exposed by the generalization" $ do
            it "at least constraint" $
              (threeChildren `isPropertySpecialization` childLeast2Constraint) `shouldBe` True
            it "at most constraint" $
              (oneChild `isPropertySpecialization` childAtmost2Constraint) `shouldBe` True
            it "exactly constraint" $
              (oneChild `isPropertySpecialization` childExactly1Constraint) `shouldBe` True
            it "mixed instances and constraints in the generalization" $
              (threeChildren `isPropertySpecialization` constPlusChild) `shouldBe` True
            it "bigger at least" $
              (getChangeExpression hasChildren [Const (Atleast 5)]
                `isPropertySpecialization`
                getChangeExpression hasChildren [Const (Atleast 4)])
              `shouldBe` True
            context "combined constraints and instances" $ do
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
          --it "the speciliazied fullfills a one of constraint"
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


              --}
              {--
    describe "isSpecialization" $ do
      context "one PrototypeDefinition is a specialization of another if" $ do
        it "all properties of the general are a changeExpressionSpecialization" $
          (carWithComputerProto `isSpecializationOf` computerProtoG) `shouldBe` True
        it "a PrototypeDefinition can be a specialization of multiple generals" $
          ((carWithComputerProto `isSpecializationOf` computerProtoG) &&
          (carWithComputerProto `isSpecializationOf` carProtoG)) `shouldBe` True
      context "one PrototypeDefintion is not a specialization of another if" $ do
        it "if the PrototypeDefinition of the special is not a fixpoint" $
          (protoDefIriToComplex protoUnfixed `isSpecializationOf` protoDefIriToComplex fixpointProto) `shouldBe` False
        it "if the PrototypeDefinition of the general is not a fixpoint" $
          (protoDefIriToComplex fixpointProto `isSpecializationOf` protoDefIriToComplex protoUnfixed) `shouldBe` False
          --}
