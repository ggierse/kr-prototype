-----------------------------------------------------------------------------
--
-- Module    :  Prototype.Basis
-- Copyright (C) 2017 Gesche Gierse
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
-----------------------------------------------------------------------------

module SpecializationSpec (spec) where
import Prototype.Specialization
import Test.Hspec
import qualified Prototype.Basis as Basis
import qualified Prototype.Composed as Composed

import TestData
import ComposedPrototypesData
import SpecializationData

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
      it "matching a cardinality constraint lower bound" $
        isMatched (Set.singleton (generateCardConstraintLower 5))
            (generateCardConstraintLower 3)
          `shouldBe` True
      it "matching a cardinality constraint upper bound" $
        isMatched (Set.singleton (generateCardConstraintUpper 5))
            (generateCardConstraintUpper 6)
          `shouldBe` True
      it "matching a cardinality constraint bounded both ways" $
        isMatched (Set.singleton (generateCardConstraint 2 5))
            (generateCardConstraint 1 6)
          `shouldBe` True
      it "matching a AllValuesFrom constraint" $
        isMatched (Set.singleton $ generateAllConstraint twoNamesSet)
            (generateAllConstraint threeNamesSet)
          `shouldBe` True
      it "matching a SomeValuesFrom constraint" $
        isMatched (Set.singleton $ generateSomeConstraint twoNamesSet)
            (generateSomeConstraint threeNamesSet)
          `shouldBe` True
      it "not matching a SomeValuesFrom constraint" $
        isMatched (Set.singleton $ generateSomeConstraint threeNamesSet)
            (generateSomeConstraint twoNamesSet)
          `shouldBe` False
      it "not matching a AllValuesFrom constraint" $
        isMatched (Set.singleton $ generateSomeConstraint threeNamesSet)
            (generateSomeConstraint twoNamesSet)
          `shouldBe` False
      it "AllValuesFrom matching an empty set as specialization constraint value" $
        isMatched (Set.singleton $ generateAllConstraint Set.empty)
            (generateAllConstraint twoNamesSet) `shouldBe` True
      it "AllValuesFrom matching empty set with empty set" $
        isMatched (Set.singleton $ generateAllConstraint Set.empty)
            (generateAllConstraint Set.empty) `shouldBe` True
      it "AllValuesFrom not matching an empty set as generalization constraint value" $
        isMatched (Set.singleton $ generateAllConstraint twoNamesSet)
            (generateAllConstraint Set.empty) `shouldBe` False
      it "not matching all and some constraint" $
        isMatched (Set.singleton $ generateAllConstraint twoNamesSet)
          (generateSomeConstraint twoNamesSet) `shouldBe` False
      it "not matching some and all constraint" $
        isMatched (Set.singleton $ generateSomeConstraint twoNamesSet)
          (generateAllConstraint twoNamesSet) `shouldBe` False
      it "not matching card and some constraint" $
        isMatched (Set.singleton $ generateCardConstraint 2 2)
          (generateSomeConstraint twoNamesSet) `shouldBe` False
      it "not matching card and all constraint" $
        isMatched (Set.singleton $ generateCardConstraint 2 2)
          (generateAllConstraint twoNamesSet) `shouldBe` False
      it "matching: find one of multiple possibilties" $
        isMatched (Set.fromList [generateSomeConstraint twoNamesSet
              , generateCardConstraint 2 5
              , generateAllConstraint twoNamesSet])
            (generateCardConstraint 1 6)
          `shouldBe` True

    describe "isPropertySpecialization" $ do
      it "a property is a specialization of another if the constraints are satisfied" $
        isPropertySpecialization hotelKB lodgingRoomPropertyWithVal lodgingRoomProperty
          `shouldBe` True
      it "a property is a specialization of another if sconsts is empty and the values are equal" $
        isPropertySpecialization hotelKB lodgingRoomPropertyWithVal lodgingRoomPropertyWithVal2
          `shouldBe` True
      it "a property is a specialization of another if the values fulfill the constraints" $
        isPropertySpecialization hotelKB lodgingRoomPropertyWithVal2 lodgingRoomProperty
          `shouldBe` True
    describe "isSpecializationOf" $
      it "example of specialization" $
        isSpecializationOf hotelKB hotelPrototype lodgingPrototype `shouldBe` True
