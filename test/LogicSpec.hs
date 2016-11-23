module LogicSpec (spec) where
import Prototype.Logic
import Test.Hspec

import Data.Map.Strict as Map

test :: IRI
test = ID "test:test"

foo :: IRI
foo = ID "test:foo"

hasName :: Property
hasName = Prop (ID "test:hasName")

name :: IRI
name = ID "test:myName"

exampleChange :: SimpleChangeExpression
exampleChange = Change hasName [name]

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

vehicleProto :: PrototypeExpression
vehicleProto = Proto {base=P0, add=[changeWheelsToFour], remove=[]}
bikeProto :: PrototypeExpression
bikeProto = Proto {base=iriToBase vehicle, add=[changeWheelsToTwo], remove = [changeWheelsToFour]}
carProto :: PrototypeExpression
carProto = Proto {base=iriToBase bike, add=[changeWheelsToFour], remove=[changeWheelsToTwo]}

bikeFixpoint :: PrototypeExpression
bikeFixpoint = Proto {base=P0, add=[changeWheelsToTwo], remove=[]}

testKB :: KnowledgeBase
testKB = KB (fromList [(vehicle, vehicleProto), (bike, bikeProto), (car, carProto)])

spec :: Spec
spec = do
    describe "isFixpoint" $ do
        it "already fixpoint" $
            isFixPoint fixpointProto `shouldBe` True
        it "not an fixpoint because of remove" $
            isFixPoint protoUnfixed `shouldBe` False
        it "not an fixpoint because of base" $
            isFixPoint protoUnfixed2 `shouldBe` False
    describe "computeFixpoint" $ do
        it "reduce to P0" $
            computeFixpoint testKB bike `shouldBe` bikeFixpoint
        it "another test" $
          True `shouldBe` True
