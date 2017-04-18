module TestData where

import Prototype.Basis
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
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

fixpointProto :: PrototypeExpression IRI
fixpointProto = Proto {idIri=test,base = P0, add = Set.empty, remove = Set.empty, remAll = Set.empty}

protoUnfixed :: PrototypeExpression IRI
protoUnfixed = Proto {idIri=test, base=P0, add = Set.empty, remove = Set.fromList [changeNameMyName], remAll = Set.empty}

protoUnfixed2 :: PrototypeExpression IRI
protoUnfixed2 = Proto {idIri=test, base = iriToBase test, add=Set.fromList [changeNameMyName], remove = Set.empty, remAll = Set.empty}

vehicle :: IRI
vehicle = ID "test:vehicle"
bike :: IRI
bike = ID "test:bike"
car :: IRI
car = ID "test:car"

numWheels :: Property
numWheels = Prop (ID "test:numWheels")
twoSet :: Set IRI
twoSet = Set.fromList [ID "2"]
fourSet :: Set IRI
fourSet = Set.fromList [ID "4"]
twoFourSet :: Set IRI
twoFourSet = Set.fromList [ID "2", ID "4"]

changeWheelsToFour :: SimpleChangeExpression
changeWheelsToFour = Change numWheels fourSet
changeWheelsToTwo :: SimpleChangeExpression
changeWheelsToTwo = Change numWheels twoSet
changeWheelsTwoFour :: SimpleChangeExpression
changeWheelsTwoFour = Change numWheels twoFourSet

vehicleProto :: PrototypeExpression IRI
vehicleProto = Proto {idIri=vehicle, base=P0, add=Set.fromList [changeWheelsToFour], remove=Set.empty, remAll = Set.empty}
bikeProto :: PrototypeExpression IRI
bikeProto = Proto {idIri=bike, base=iriToBase vehicle, add=Set.fromList [changeWheelsToTwo], remove = Set.fromList [changeWheelsToFour], remAll = Set.empty}
carProtoDef :: PrototypeExpression IRI
carProtoDef = Proto {idIri=car, base=iriToBase bike, add=Set.fromList [changeWheelsToFour], remove=Set.fromList [changeWheelsToTwo], remAll = Set.empty}

bikeFixpoint :: PrototypeExpression IRI
bikeFixpoint = Proto {idIri=bike, base=P0, add=Set.fromList [changeWheelsToTwo], remove=Set.empty, remAll = Set.empty}

carFixpoint :: PrototypeExpression IRI
carFixpoint = Proto {idIri=car, base=P0, add=Set.fromList [changeWheelsToFour], remove=Set.empty, remAll = Set.empty}

testKB :: KnowledgeBase IRI
testKB = generateKBfromPrototypeExps [vehicleProto, bikeProto, carProtoDef]

testKBFixed :: KnowledgeBase IRI
testKBFixed = generateKBfromPrototypeExps [vehicleProto, bikeFixpoint, carFixpoint]


mapTwo :: Map Property (Set IRI)
mapTwo = Map.fromList [(numWheels, Set.fromList [ID "4", ID "2"])]
mapOne :: Map Property (Set IRI)
mapOne = Map.fromList [(numWheels, Set.fromList [ID "4"])]


mapTwoProperties :: Map Property (Set IRI)
mapTwoProperties = Map.fromList [(numWheels, twoFourSet), (hasName, Set.fromList [myName, test])]
mapTwoPropertiesOneEach :: Map Property (Set IRI)
mapTwoPropertiesOneEach = Map.fromList [(numWheels, fourSet), (hasName, Set.fromList [test])]



getAddProtoDef :: (Ord a) => IRI -> [ChangeExpression a] -> PrototypeExpression a
getAddProtoDef ident changes = Proto
  { idIri = ident
  , base = P0
  , add = Set.fromList changes
  , remove = Set.empty
  , remAll = Set.empty}

engineSize :: Property
engineSize = Prop (ID "engineSize")
hasWheels :: Property
hasWheels = Prop (ID "hasWheels")

parent :: IRI
parent = ID "family:parent"
hasChildren :: Property
hasChildren = Prop (ID "family:hasChildren")

frank :: IRI
frank = ID "family:frank"
jan :: IRI
jan = ID "family:jan"
susan :: IRI
susan = ID "family:susan"
lars :: IRI
lars = ID "family:lars"
tad :: IRI
tad = ID "family:tad"
tamara :: IRI
tamara = ID "family:tamara"
