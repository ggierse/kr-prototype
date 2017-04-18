module ComposedSpec (spec) where
import Prototype.Composed
import Test.Hspec
import qualified Prototype.Basis as Basis

import TestData
import ComposedPrototypesData as CData

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
    it "another iri returns Nothing" $
      convertIriToConstName jan `shouldBe` Nothing
  describe "isTypeConstraintPrototype" $ do
    it "is true for a valid type constraint" $
      isTypeConstraintPrototype namesAllFromConstraint `shouldBe` True
    it "is false for a property prototype" $
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
  describe "convertCardConstProto" $ do
    it "convert example [2, infty) constraint correctly" $
      convertCardConstProto CData.childLeast2Constraint `shouldBe` Just (generateCardConstraintLower 2)
    it "converts example [3,5] constraint correctly" $
      convertCardConstProto cardBothDefConstraint `shouldBe` Just (generateCardConstraint 3 5)
    it "returns nothing for faulty constraint prototype" $
      convertCardConstProto noLowerConst `shouldBe` Nothing
  describe "const of a composed prototype looks up the constraint prototypes and transforms them to ConstraintInfo" $ do
    it "single constraint with allValuesFrom" $
      consts fkb mixedProperty `shouldBe` Set.fromList [generateAllConstraint nameSet]
    it "single constraint with someValuesFrom" $
      consts fkb someProperty `shouldBe` Set.fromList [generateSomeConstraint nameSet]
    it "single constraint with cardinality [2,infinity]" $
      consts fkb childLeast2Property `shouldBe`
        Set.fromList [generateCardConstraintLower 2]
