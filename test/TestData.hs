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

fixpointProto :: PrototypeDefinition IRI
fixpointProto = Proto {base = P0, add = Set.empty, remove = Set.empty, remAll = Set.empty}

protoUnfixed :: PrototypeDefinition IRI
protoUnfixed = Proto {base=P0, add = Set.empty, remove = Set.fromList [changeNameMyName], remAll = Set.empty}

protoUnfixed2 :: PrototypeDefinition IRI
protoUnfixed2 = Proto {base = iriToBase test, add=Set.fromList [changeNameMyName], remove = Set.empty, remAll = Set.empty}

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

vehicleProto :: PrototypeDefinition IRI
vehicleProto = Proto {base=P0, add=Set.fromList [changeWheelsToFour], remove=Set.empty, remAll = Set.empty}
bikeProto :: PrototypeDefinition IRI
bikeProto = Proto {base=iriToBase vehicle, add=Set.fromList [changeWheelsToTwo], remove = Set.fromList [changeWheelsToFour], remAll = Set.empty}
carProto :: PrototypeDefinition IRI
carProto = Proto {base=iriToBase bike, add=Set.fromList [changeWheelsToFour], remove=Set.fromList [changeWheelsToTwo], remAll = Set.empty}

bikeFixpoint :: PrototypeDefinition IRI
bikeFixpoint = Proto {base=P0, add=Set.fromList [changeWheelsToTwo], remove=Set.empty, remAll = Set.empty}

carFixpoint :: PrototypeDefinition IRI
carFixpoint = Proto {base=P0, add=Set.fromList [changeWheelsToFour], remove=Set.empty, remAll = Set.empty}

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




getChangeExpression :: Property -> [Special.ComplexValue] -> ChangeExpression Special.ComplexValue
getChangeExpression prop values = Change prop (Set.fromList values)


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

constPlusChild :: ChangeExpression Special.ComplexValue
constPlusChild = Change hasChildren (Set.fromList [Special.Const (Special.Atleast 3), Special.Value frank])

frankProto :: PrototypeDefinition IRI
frankProto = Proto {base=P0, add=Set.singleton (Change hasChildren (Set.singleton jan)), remove=Set.empty, remAll = Set.empty}
janProto :: PrototypeDefinition propValueType
janProto = Proto {base=P0, add=Set.empty, remove=Set.empty, remAll = Set.empty}
tadProto :: PrototypeDefinition IRI
tadProto = Proto {
  base=P0,
  add=Set.singleton (Change hasChildren (Set.fromList [frank, tamara, susan])),
  remove=Set.empty,
  remAll = Set.empty
}

childLeast2Constraint :: ChangeExpression Special.ComplexValue
childLeast2Constraint = Change hasChildren (Set.singleton (Special.Const (Special.Atleast 2)))
childAtmost2Constraint :: ChangeExpression Special.ComplexValue
childAtmost2Constraint = Change hasChildren (Set.singleton (Special.Const (Special.Atmost 2)))
childExactly1Constraint :: ChangeExpression Special.ComplexValue
childExactly1Constraint = Change hasChildren (Set.singleton (Special.Const (Special.Exactly 1)))


parentProto :: PrototypeDefinition Special.ComplexValue
parentProto = Proto {base=P0, add=Set.singleton childLeast2Constraint, remove=Set.empty, remAll = Set.empty}

{--
familyKB :: Map.Map IRI (PrototypeDefinition Special.ComplexValue)
familyKB = Map.fromList [(parent, parentProto), (frank, frankProto), (jan, janProto), (tad,tadProto)]
--}
