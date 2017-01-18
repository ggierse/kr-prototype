module TestData where

import Prototype.Basis
import qualified Prototype.Specialization as Special
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

carProtoG :: PrototypeExpression Special.ComplexValue
carProtoG =
  getAddProtoDef
    car
    [ Special.getChangeExpression hasWheels [Special.Const (Special.Atleast 3), Special.Const (Special.Atmost 5)]
    , Special.getChangeExpression engineSize [Special.Value (ID "8.5l")]]

computer :: IRI
computer = ID "example:computer"
hasRam :: Property
hasRam = Prop (ID "example:hasRam")
hasCPU :: Property
hasCPU = Prop (ID "example:hasCPU")

computerProtoG :: PrototypeExpression Special.ComplexValue
computerProtoG =
  getAddProtoDef
  computer
  [ Special.getChangeExpression hasRam [Special.Const (Special.Atleast 1)]
  , Special.getChangeExpression hasCPU [Special.Const (Special.Atleast 1)]
  ]


compcar :: IRI
compcar = ID "example:compcar"
carWithComputerProto :: PrototypeExpression Special.ComplexValue
carWithComputerProto =
  getAddProtoDef
  compcar
  [ Special.getChangeExpression hasWheels [Special.Value $ ID "wheelOne", Special.Value $ ID "wheelTwo", Special.Value $ ID "wheelThree", Special.Value $ ID "wheelFour"]
  ,  Special.getChangeExpression engineSize [Special.Value (ID "8.5l")]
  ,  Special.getChangeExpression hasRam [Special.Const $ Special.Atleast 2]
  ,  Special.getChangeExpression hasCPU [Special.Value $ ID "superCPU1", Special.Value $ ID "superCPU2"]
  ]


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

frankProto :: PrototypeExpression IRI
frankProto = Proto {idIri=frank, base=P0, add=Set.singleton (Change hasChildren (Set.singleton jan)), remove=Set.empty, remAll = Set.empty}
janProto :: PrototypeExpression propValueType
janProto = Proto {idIri=jan, base=P0, add=Set.empty, remove=Set.empty, remAll = Set.empty}
tadProto :: PrototypeExpression IRI
tadProto =
  Proto { idIri=tad
        , base=P0
        , add=Set.singleton (Change hasChildren (Set.fromList [frank, tamara, susan]))
        , remove=Set.empty
        , remAll = Set.empty
      }

childLeast2Constraint :: ChangeExpression Special.ComplexValue
childLeast2Constraint = Change hasChildren (Set.singleton (Special.Const (Special.Atleast 2)))
childAtmost2Constraint :: ChangeExpression Special.ComplexValue
childAtmost2Constraint = Change hasChildren (Set.singleton (Special.Const (Special.Atmost 2)))
childExactly1Constraint :: ChangeExpression Special.ComplexValue
childExactly1Constraint = Change hasChildren (Set.singleton (Special.Const (Special.Exactly 1)))
childExactly2Constraint :: ChangeExpression Special.ComplexValue
childExactly2Constraint = Change hasChildren (Set.singleton (Special.Const (Special.Exactly 2)))


mixedAtLeast3Iri1 :: ChangeExpression Special.ComplexValue
mixedAtLeast3Iri1 = Special.getChangeExpression hasChildren [Special.Const (Special.Atleast 3), Special.Value frank]
mixedAtMost2Iri1 :: ChangeExpression Special.ComplexValue
mixedAtMost2Iri1 = Special.getChangeExpression hasChildren [Special.Const (Special.Atmost 2), Special.Value frank]
mixedAtMost2Iri2 :: ChangeExpression Special.ComplexValue
mixedAtMost2Iri2 = Special.getChangeExpression hasChildren [Special.Const (Special.Atmost 2), Special.Value frank, Special.Value tad]
mixedAtMost2Iri3 :: ChangeExpression Special.ComplexValue
mixedAtMost2Iri3 = Special.getChangeExpression hasChildren [Special.Const (Special.Atmost 2), Special.Value frank, Special.Value tad, Special.Value lars]


parentProto :: PrototypeExpression Special.ComplexValue
parentProto = Proto {idIri=parent, base=P0, add=Set.singleton childLeast2Constraint, remove=Set.empty, remAll = Set.empty}



{--
familyKB :: Map IRI (PrototypeExpression Special.ComplexValue)
familyKB = Map.fromList [(parent, parentProto), (frank, frankProto), (jan, janProto), (tad,tadProto)]
--}
