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


module BasisSpec (spec) where
import Prototype.Basis
import Test.Hspec
import TestData

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Exception as Exception

spec :: Spec
spec = do
    describe "Fixpoint related tests" $ do
      describe "isFixpoint" $ do
          it "already fixpoint" $
              isFixPoint fixpointProto `shouldBe` True
          it "not an fixpoint because of remove" $
              isFixPoint protoUnfixed `shouldBe` False
          it "not an fixpoint because of base" $
              isFixPoint protoUnfixed2 `shouldBe` False
      describe "getBranchToP0" $ do
        it "P0 is one step away" $
          length (getBranchToP0 testKB bikeProto) `shouldBe` 2
        it "branch order is correct for one steps" $
          getBranchToP0 testKB bikeProto `shouldBe` [bikeProto, vehicleProto]
        it "P0 is two steps away" $
          length (getBranchToP0 testKB carProtoDef) `shouldBe` 3
        it "branch order is correct for two steps" $
          getBranchToP0 testKB carProtoDef `shouldBe` [carProtoDef, bikeProto, vehicleProto]

      describe "removeProperty" $ do
        it "remove one iri of two" $
          removeProperty mapTwo changeWheelsToTwo `shouldBe` mapOne
        it "remove all iris" $
          removeProperty mapTwo changeWheelsTwoFour `shouldBe` Map.empty

      describe "removeAllProps" $
        it "remove all iris of a named property" $
          removeAllProps mapTwo (Set.singleton numWheels) `shouldBe` Map.empty

      describe "removeProperties" $ do
        it "remove one iri from two differen properties" $
          removeProperties mapTwoProperties (Set.fromList [changeWheelsToTwo, changeNameMyName]) `shouldBe` mapTwoPropertiesOneEach
        it "remove two properties completely" $
          removeProperties mapTwoProperties (Set.fromList [changeWheelsTwoFour, changeNameMyName, changeNameTest])
          `shouldBe` Map.empty

      describe "addProperty" $ do
        it "add one iri to PropertyMap" $
          addProperty mapOne changeWheelsToTwo `shouldBe` mapTwo
        it "add multiple iris to empty PropertyMap" $
          addProperty Map.empty changeWheelsTwoFour `shouldBe` mapTwo

      describe "addProperties" $
        it "add one iri to two different properties each" $
          addProperties mapTwoPropertiesOneEach (Set.fromList [changeWheelsToTwo, changeNameMyName]) `shouldBe` mapTwoProperties

      describe "applyPrototypeExpression" $ do
        it "add one prototype expression to prototype" $
          let basePrototype = PT {name=bike, props=Map.fromList [(numWheels, fourSet)]}
              protoExpression = Proto {idIri=bike, base=P0, add=Set.singleton changeWheelsToTwo, remove=Set.singleton changeWheelsToFour, remAll=Set.empty}
              expectedPrototype = PT {name=bike, props=Map.singleton numWheels twoSet}
          in applyPrototypeExpression protoExpression basePrototype`shouldBe` expectedPrototype
        it "remove everything from prototype" $
          let basePrototype = PT {name=bike, props=Map.fromList [(numWheels, twoFourSet)]}
              protoExpression = Proto {idIri=bike, base = P0, add =Set.empty, remove = Set.fromList [changeWheelsToFour, changeWheelsToTwo], remAll=Set.empty}
              expectedPrototype = PT {name=bike, props=Map.empty}
          in applyPrototypeExpression protoExpression basePrototype `shouldBe` expectedPrototype

      describe "branchToPrototype" $ do
        it "branch with one item" $
          branchToPrototype vehicle [vehicleProto] `shouldBe` PT {name=vehicle, props=Map.fromList[(numWheels, fourSet)]}
        it "branch with two items" $
          branchToPrototype car [carProtoDef, bikeProto, vehicleProto] `shouldBe` PT {name=car, props=Map.fromList[(numWheels, fourSet)]}

      describe "computeFixpoint" $ do
        it "reduce to P0" $
          computeFixpoint testKB bike `shouldBe` bikeFixpoint
        it "another test" $
          computeFixpoint testKB car `shouldBe` carFixpoint
        it "already fixpoint should still be fixpoint" $
          computeFixpoint testKB vehicle `shouldBe` testKB Map.! vehicle

      describe "computeAllFixpoints" $
        it "compute fixpoints for simple knowledge base" $
          computeAllFixpoints testKB `shouldBe` testKBFixed

    describe "consistency check related tests" $ do
      it "isMapKeySameAsProtoID throws error" $
        evaluate (isMapKeySameAsProtoID True car fixpointProto) `shouldThrow` keyException
      it "not consistent if kb keys are not equal to proto ids" $
        evaluate ( (checkConsistency (Map.singleton car bikeProto))) `shouldThrow` keyException


keyException :: Selector ConsistencyException
keyException (KeysInconsistent _) = True
keyException _ = False


cycleException :: Selector ConsistencyException
cycleException (CycleDetected _) = True
cycleException _ = False
