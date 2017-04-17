module SpecializationSpec (spec) where
import Prototype.Specialization
import Test.Hspec
import qualified Prototype.Basis as Basis

import TestData
import ComposedPrototypesData as CData

-- import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntegerInterval
import Control.Exception

spec :: Spec
spec = do
    describe "composed prototypes basics" $ do
      it "properties of a composed prototype are the set of prototypes that results from looking up the values of proto:hasProperty" $
        properties fkb genProto `shouldBe` Set.fromList [childLeast2Property, namesAllFromProperty, mixedProperty]
      it "val of a composed prototype looks up the values of the property proto:hasValue" $
        val mixedProperty `shouldBe` mixedValues
      describe "constraint conversion from iri works" $ do
        it "allValuesFrom" $
          convertIriToConstName allValuesFrom `shouldBe` Just AllValuesFrom
        it "someValuesFrom" $
          convertIriToConstName someValuesFrom `shouldBe` Just SomeValuesFrom
        it "another iri" $
          convertIriToConstName jan `shouldBe` Nothing
      it "isTypeConstraintPrototype is true for a valid type constraint" $
        isTypeConstraintPrototype namesAllFromConstraint `shouldBe` True
      it "isTypeConstraintPrototype is false for a property prototype" $
        isTypeConstraintPrototype namesAllFromProperty `shouldBe` False
      describe "ordering from ConstraintInfo works" $ do
        it "inserting things with same type is ok" $
          Set.fromList [generateAllConstraint nameSet, generateAllConstraint Set.empty] `shouldBe`
            Set.fromList [generateAllConstraint nameSet, generateAllConstraint Set.empty]
        it "inserting different intervals is ok" $
          Set.fromList [generateCardConstraintLower 2,generateCardConstraint 2 5]
            `shouldBe` Set.fromList [generateCardConstraintLower 2, generateCardConstraint 2 5]
        it "inserting same interval does not lead to dublication" $
          Set.fromList [generateCardConstraintLower 2, generateCardConstraintLower 2]
            `shouldBe` Set.fromList [generateCardConstraintLower 2]
        describe "convert iris to integers" $ do
          it "convert plain string" $
            convertIriToInteger (Basis.ID "2") `shouldBe` Just 2
          it "nothing if not integer" $
            convertIriToInteger (Basis.ID "someString") `shouldBe` Nothing
        describe "isCardConstraintPrototype" $ do
          it "true for proto with concrete lower and upper is infty" $
            isCardConstraintPrototype CData.childLeast2Constraint `shouldBe` True
          it "true for proto with concrete lower and upper" $
            isCardConstraintPrototype cardBothDefConstraint `shouldBe` True
          it "false for proto without lower" $
            isCardConstraintPrototype noLowerConst `shouldBe` False
          it "false for proto without upper" $
            isCardConstraintPrototype noUpperConst `shouldBe` False
          it "false if lower val is not an integer" $
            isCardConstraintPrototype lowNotIntConst `shouldBe` False
          it "false if upper val is not an integer and not proto:infty" $
            isCardConstraintPrototype upNotIntConst `shouldBe` False
          it "true if upper val proto:infty" $
            isCardConstraintPrototype upInftyConst `shouldBe` True
        describe "parseInterval" $ do
          it "parseInterval converts two number iris to an integer interval" $
            parseInterval (Basis.ID "2") (Basis.ID "5") `shouldBe` Finite 2  <=..<= Finite 5
          it "parseInterval can handle proto:infty" $
            parseInterval (Basis.ID "10") (Basis.ID "proto:infty") `shouldBe` Finite 10  <=..< PosInf
          it "parseInterval throws error on faulty values" $
            evaluate (parseInterval (Basis.ID "not correct") (Basis.ID "5")) `shouldThrow` anyErrorCall
      describe "const of a composed prototype looks up the constraint prototypes and transforms them to ConstraintInfo" $ do
        it "single constraint with allValuesFrom" $
          consts fkb mixedProperty `shouldBe` Set.fromList [generateAllConstraint nameSet]
        it "single constraint with someValuesFrom" $
          consts fkb someProperty `shouldBe` Set.fromList [generateSomeConstraint nameSet]
        it "single constraint with cardinality [2,infinity]" $
          consts fkb childLeast2Property `shouldBe`
            Set.fromList [generateCardConstraintLower 2]
    describe "iris and constraints" $ do
      it "a set of iris is a specialization of an atleast constraint" $
        Set.fromList [jan, susan] `isSpecializationOf` Atleast 1 `shouldBe` True
      it "a set of iris is a specialization of an atmost constraint" $
        Set.fromList [jan, susan] `isSpecializationOf` Atmost 5 `shouldBe` True
      it "a set of iris is a specialization of an exactly constraint" $
        Set.fromList [jan, susan] `isSpecializationOf` Exactly 2 `shouldBe` True
    describe "isSpecializationOf" $ do
      --it "a Constraint is a specialization of another Constraint if the numbers fit" $
      --  Const (Atleast 5) `isSpecializationOf` Const (Atleast 3)
      it "an empty set is a specialization of an empty set" $
        (Set.empty :: Set Basis.IRI) `isSpecializationOf` (Set.empty :: Set Basis.IRI)
      it "if only constraints are used the definition for iris does not hinder the implementation" $ -- TODO: this test is to much dependant on the implementation
        getIris (Set.singleton (Const (Atleast 5))) `isSpecializationOf` getIris (Set.singleton (Const (Atleast 3)))
      it "a set of constraints is a specialization of a constraint" $
        Set.singleton (Atleast 5)  `isSpecializationOf` Atleast 3
    describe "changeExpressionIsSpecialization" $ do
        context "one change expression is a specialization of another if" $ do
          it "they are equal" $
            isSpecializationOf changeNameMyName changeNameMyName `shouldBe` True
          it "the specialized is a superset of the generalization" $
            isSpecializationOf changeWheelsTwoFour changeWheelsToTwo `shouldBe` True
          context "the specialized fullfills a number constraint exposed by the generalization" $ do
            it "at least constraint" $
              (threeChildren `isSpecializationOf` TestData.childLeast2Constraint) `shouldBe` True
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
            context "combined constraints and instances" $ do
              it "atleast constraint alone should make it specialization" $
                (mixedAtLeast3Iri1 `isSpecializationOf` TestData.childLeast2Constraint) `shouldBe` True
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
               oneChild `isSpecializationOf` TestData.childLeast2Constraint `shouldBe` False
            it "at most constraint" $
              (threeChildren `isSpecializationOf`  childAtmost2Constraint) `shouldBe` False
            it "exactly constraint" $
              (threeChildren `isSpecializationOf`  childExactly1Constraint) `shouldBe` False
            it "exactly constraint cannot be specialized by constraint" $
              (childExactly1Constraint `isSpecializationOf` childExactly2Constraint) `shouldBe` False



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
