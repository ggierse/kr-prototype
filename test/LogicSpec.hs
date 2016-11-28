module LogicSpec (spec) where
import Prototype.Logic
import Test.Hspec

import Data.Map.Strict as Map

test :: IRI
test = ID "test:test"

hasName :: Property
hasName = Prop (ID "test:hasName")

myName :: IRI
myName = ID "test:myName"

exampleChange :: SimpleChangeExpression
exampleChange = Change hasName [myName]

fixpointProto :: PrototypeExpression
fixpointProto = Proto {base = P0, add = [], remove = []}

protoUnfixed :: PrototypeExpression
protoUnfixed = Proto {base=P0, add = [], remove = [exampleChange]}

protoUnfixed2 :: PrototypeExpression
protoUnfixed2 = Proto {base = iriToBase test, add=[exampleChange], remove = []}

vehicle :: IRI
vehicle = ID "test:vehicle"
bike :: IRI
bike = ID "test:bike"
car :: IRI
car = ID "test:car"

numWheels :: Property
numWheels = Prop (ID "test:numWheels")
changeWheelsToFour :: SimpleChangeExpression
changeWheelsToFour = Change numWheels [ID "4"]
changeWheelsToTwo :: SimpleChangeExpression
changeWheelsToTwo = Change numWheels[ID "2"]
changeWheelsToNull :: SimpleChangeExpression
changeWheelsToNull = Change numWheels[ID "2", ID "4"]

vehicleProto :: PrototypeExpression
vehicleProto = Proto {base=P0, add=[changeWheelsToFour], remove=[]}
bikeProto :: PrototypeExpression
bikeProto = Proto {base=iriToBase vehicle, add=[changeWheelsToTwo], remove = [changeWheelsToFour]}
carProto :: PrototypeExpression
carProto = Proto {base=iriToBase bike, add=[changeWheelsToFour], remove=[changeWheelsToTwo]}

bikeFixpoint :: PrototypeExpression
bikeFixpoint = Proto {base=P0, add=[changeWheelsToTwo], remove=[]}

testKB :: KnowledgeBase
testKB = fromList [(vehicle, vehicleProto), (bike, bikeProto), (car, carProto)]

mapTwo :: Map Property [IRI]
mapTwo = fromList [(numWheels, [ID "4", ID "2"])]
mapOne :: Map Property [IRI]
mapOne = fromList [(numWheels, [ID "4"])]

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
        length (getBranchToP0 testKB carProto) `shouldBe` 3
      it "branch order is correct for two steps" $
        getBranchToP0 testKB carProto `shouldBe` [carProto, bikeProto, vehicleProto]

    describe "removeProperty" $ do
      it "remove one of two" $
        removeProperty mapTwo changeWheelsToTwo `shouldBe` mapOne
      it "remove all" $
        removeProperty mapTwo changeWheelsToNull `shouldBe` empty

    describe "branchToPrototype" $ do
      it "branch with one item" $
        branchToPrototype vehicle [vehicleProto] `shouldBe` PT {name=vehicle, properties=(fromList[(numWheels, [ID "4"])])}

    describe "computeFixpoint" $ do
        it "reduce to P0" $
            computeFixpoint testKB bike `shouldBe` bikeFixpoint
        it "another test" $
          True `shouldBe` True
