module TestData where

import Prototype.Basis
import qualified Data.Set as Set
import qualified Data.Map as Map

test :: IRI
test = ID "test:test"

hasName :: Property
hasName = Prop (ID "test:hasName")
myName :: IRI
myName = ID "test:myName"

changeNameMyName :: SimpleChangeExpression
changeNameMyName = Change hasName (Set.fromList [myName])
changeNameTest :: SimpleChangeExpression
changeNameTest = Change hasName (Set.fromList [test])
changeWheelsMyName :: SimpleChangeExpression
changeWheelsMyName = Change numWheels (Set.singleton myName)

fixpointProto :: PrototypeExpression
fixpointProto = Proto {base = P0, add = Set.empty, remove = Set.empty}

protoUnfixed :: PrototypeExpression
protoUnfixed = Proto {base=P0, add = Set.empty, remove = Set.fromList [changeNameMyName]}

protoUnfixed2 :: PrototypeExpression
protoUnfixed2 = Proto {base = iriToBase test, add=Set.fromList [changeNameMyName], remove = Set.empty}

vehicle :: IRI
vehicle = ID "test:vehicle"
bike :: IRI
bike = ID "test:bike"
car :: IRI
car = ID "test:car"

numWheels :: Property
numWheels = Prop (ID "test:numWheels")
twoSet :: Set.Set IRI
twoSet = Set.fromList [ID "2"]
fourSet :: Set.Set IRI
fourSet = Set.fromList [ID "4"]
twoFourSet :: Set.Set IRI
twoFourSet = Set.fromList [ID "2", ID "4"]

changeWheelsToFour :: SimpleChangeExpression
changeWheelsToFour = Change numWheels fourSet
changeWheelsToTwo :: SimpleChangeExpression
changeWheelsToTwo = Change numWheels twoSet
changeWheelsTwoFour :: SimpleChangeExpression
changeWheelsTwoFour = Change numWheels twoFourSet

vehicleProto :: PrototypeExpression
vehicleProto = Proto {base=P0, add=Set.fromList [changeWheelsToFour], remove=Set.empty}
bikeProto :: PrototypeExpression
bikeProto = Proto {base=iriToBase vehicle, add=Set.fromList [changeWheelsToTwo], remove = Set.fromList [changeWheelsToFour]}
carProto :: PrototypeExpression
carProto = Proto {base=iriToBase bike, add=Set.fromList [changeWheelsToFour], remove=Set.fromList [changeWheelsToTwo]}

bikeFixpoint :: PrototypeExpression
bikeFixpoint = Proto {base=P0, add=Set.fromList [changeWheelsToTwo], remove=Set.empty}

carFixpoint :: PrototypeExpression
carFixpoint = Proto {base=P0, add=Set.fromList [changeWheelsToFour], remove=Set.empty}

testKB :: KnowledgeBase
testKB = Map.fromList [(vehicle, vehicleProto), (bike, bikeProto), (car, carProto)]

testKBFixed :: Map.Map IRI PrototypeExpression
testKBFixed = Map.fromList [(vehicle, vehicleProto), (bike, bikeFixpoint), (car, carFixpoint)]


mapTwo :: Map.Map Property (Set.Set IRI)
mapTwo = Map.fromList [(numWheels, Set.fromList [ID "4", ID "2"])]
mapOne :: Map.Map Property (Set.Set IRI)
mapOne = Map.fromList [(numWheels, Set.fromList [ID "4"])]


mapTwoProperties :: Map.Map Property (Set.Set IRI)
mapTwoProperties = Map.fromList [(numWheels, twoFourSet), (hasName, Set.fromList [myName, test])]
mapTwoPropertiesOneEach :: Map.Map Property (Set.Set IRI)
mapTwoPropertiesOneEach = Map.fromList [(numWheels, fourSet), (hasName, Set.fromList [test])]
