module BasisSpec (spec) where
import Prototype.Basis
import Test.Hspec
import TestData

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


spec :: Spec
spec = do
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

    describe "applyPrototypeDefinition" $ do
      it "add one prototype expression to prototype" $
        let basePrototype = PT {name=bike, properties=Map.fromList [(numWheels, fourSet)]}
            protoExpression = Proto {base=P0, add=Set.singleton changeWheelsToTwo, remove=Set.singleton changeWheelsToFour, remAll=Set.empty}
            expectedPrototype = PT {name=bike, properties=Map.singleton numWheels twoSet}
        in applyPrototypeDefinition protoExpression basePrototype`shouldBe` expectedPrototype
      it "remove everything from prototype" $
        let basePrototype = PT {name=bike, properties=Map.fromList [(numWheels, twoFourSet)]}
            protoExpression = Proto {base = P0, add =Set.empty, remove = Set.fromList [changeWheelsToFour, changeWheelsToTwo], remAll=Set.empty}
            expectedPrototype = PT {name=bike, properties=Map.empty}
        in applyPrototypeDefinition protoExpression basePrototype `shouldBe` expectedPrototype

    describe "branchToPrototype" $ do
      it "branch with one item" $
        branchToPrototype vehicle [vehicleProto] `shouldBe` PT {name=vehicle, properties=Map.fromList[(numWheels, fourSet)]}
      it "branch with two items" $
        branchToPrototype car [carProtoDef, bikeProto, vehicleProto] `shouldBe` PT {name=car, properties=Map.fromList[(numWheels, fourSet)]}

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
