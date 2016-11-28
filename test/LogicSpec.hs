module LogicSpec (spec) where
import Prototype.Logic
import Test.Hspec

import Data.Map.Strict as Map
import qualified Data.Set as Set

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

testKB :: KnowledgeBase
testKB = fromList [(vehicle, vehicleProto), (bike, bikeProto), (car, carProto)]

mapTwo :: Map Property (Set.Set IRI)
mapTwo = fromList [(numWheels, Set.fromList [ID "4", ID "2"])]
mapOne :: Map Property (Set.Set IRI)
mapOne = fromList [(numWheels, Set.fromList [ID "4"])]


mapTwoProperties :: Map Property (Set.Set IRI)
mapTwoProperties = fromList [(numWheels, twoFourSet), (hasName, Set.fromList [myName, test])]
mapTwoPropertiesOneEach :: Map Property (Set.Set IRI)
mapTwoPropertiesOneEach = fromList [(numWheels, fourSet), (hasName, Set.fromList [test])]

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
      it "remove one iri of two" $
        removeProperty mapTwo changeWheelsToTwo `shouldBe` mapOne
      it "remove all iris" $
        removeProperty mapTwo changeWheelsTwoFour `shouldBe` empty

    describe "removeProperties" $ do
      it "remove one iri from two differen properties" $
        removeProperties mapTwoProperties (Set.fromList [changeWheelsToTwo, changeNameMyName]) `shouldBe` mapTwoPropertiesOneEach
      it "remove two properties completely" $
        removeProperties mapTwoProperties (Set.fromList [changeWheelsTwoFour, changeNameMyName, changeNameTest])
        `shouldBe` empty

    describe "addProperty" $ do
      it "add one iri to PropertyMap" $
        addProperty mapOne changeWheelsToTwo `shouldBe` mapTwo
      it "add multiple iris to empty PropertyMap" $
        addProperty empty changeWheelsTwoFour `shouldBe` mapTwo

    describe "addProperties" $
      it "add one iri to two different properties each" $
        addProperties mapTwoPropertiesOneEach [changeWheelsToTwo, changeNameMyName] `shouldBe` mapTwoProperties

    describe "branchToPrototype" $ do
      it "branch with one item" $
        branchToPrototype vehicle [vehicleProto] `shouldBe` PT {name=vehicle, properties=fromList[(numWheels, fourSet)]}

    describe "computeFixpoint" $ do
        it "reduce to P0" $
            computeFixpoint testKB bike `shouldBe` bikeFixpoint
        it "another test" $
          True `shouldBe` True
