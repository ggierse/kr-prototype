module TestData where

import Prototype.Basis
import qualified Prototype.Specialization as Special
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

fixpointProto :: PrototypeExpression IRI
fixpointProto = Proto {base = P0, add = Set.empty, remove = Set.empty}

protoUnfixed :: PrototypeExpression IRI
protoUnfixed = Proto {base=P0, add = Set.empty, remove = Set.fromList [changeNameMyName]}

protoUnfixed2 :: PrototypeExpression IRI
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

vehicleProto :: PrototypeExpression IRI
vehicleProto = Proto {base=P0, add=Set.fromList [changeWheelsToFour], remove=Set.empty}
bikeProto :: PrototypeExpression IRI
bikeProto = Proto {base=iriToBase vehicle, add=Set.fromList [changeWheelsToTwo], remove = Set.fromList [changeWheelsToFour]}
carProto :: PrototypeExpression IRI
carProto = Proto {base=iriToBase bike, add=Set.fromList [changeWheelsToFour], remove=Set.fromList [changeWheelsToTwo]}

bikeFixpoint :: PrototypeExpression IRI
bikeFixpoint = Proto {base=P0, add=Set.fromList [changeWheelsToTwo], remove=Set.empty}

carFixpoint :: PrototypeExpression IRI
carFixpoint = Proto {base=P0, add=Set.fromList [changeWheelsToFour], remove=Set.empty}

testKB :: KnowledgeBase IRI
testKB = Map.fromList [(vehicle, vehicleProto), (bike, bikeProto), (car, carProto)]

testKBFixed :: KnowledgeBase IRI
testKBFixed = Map.fromList [(vehicle, vehicleProto), (bike, bikeFixpoint), (car, carFixpoint)]


mapTwo :: Map.Map Property (Set.Set IRI)
mapTwo = Map.fromList [(numWheels, Set.fromList [ID "4", ID "2"])]
mapOne :: Map.Map Property (Set.Set IRI)
mapOne = Map.fromList [(numWheels, Set.fromList [ID "4"])]


mapTwoProperties :: Map.Map Property (Set.Set IRI)
mapTwoProperties = Map.fromList [(numWheels, twoFourSet), (hasName, Set.fromList [myName, test])]
mapTwoPropertiesOneEach :: Map.Map Property (Set.Set IRI)
mapTwoPropertiesOneEach = Map.fromList [(numWheels, fourSet), (hasName, Set.fromList [test])]



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

oneChild :: ChangeExpression Special.ComplexValue
oneChild = Change hasChildren (Set.singleton (Special.Value jan))
threeChildren :: ChangeExpression Special.ComplexValue
threeChildren = Change hasChildren (Set.fromList [Special.Value frank, Special.Value tamara, Special.Value susan])


frankProto :: PrototypeExpression IRI
frankProto = Proto {base=P0, add=Set.singleton (Change hasChildren (Set.singleton jan)), remove=Set.empty}
janProto :: PrototypeExpression propValueType
janProto = Proto {base=P0, add=Set.empty, remove=Set.empty}
tadProto :: PrototypeExpression IRI
tadProto = Proto {
  base=P0,
  add=Set.singleton (Change hasChildren (Set.fromList [frank, tamara, susan])),
  remove=Set.empty}

childLeast2Constraint :: ChangeExpression Special.ComplexValue
childLeast2Constraint = Change hasChildren (Set.singleton (Special.Const (Special.Atleast 2)))
childAtmost2Constraint :: ChangeExpression Special.ComplexValue
childAtmost2Constraint = Change hasChildren (Set.singleton (Special.Const (Special.Atmost 2)))
childExactly1Constraint :: ChangeExpression Special.ComplexValue
childExactly1Constraint = Change hasChildren (Set.singleton (Special.Const (Special.Exactly 1)))

parentProto :: PrototypeExpression Special.ComplexValue
parentProto = Proto {base=P0, add=Set.singleton childLeast2Constraint, remove=Set.empty}

{--
familyKB :: Map.Map IRI (PrototypeExpression Special.ComplexValue)
familyKB = Map.fromList [(parent, parentProto), (frank, frankProto), (jan, janProto), (tad,tadProto)]
--}
