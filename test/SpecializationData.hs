module SpecializationData where

import Prototype.Basis
import Prototype.Composed
import qualified Prototype.Specialization as Special
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import TestData
import ComposedPrototypesData


lodging :: IRI
lodging = ID "hotel:lodging"
hostel :: IRI
hostel = ID "hotel:hostel"
hotel :: IRI
hotel = ID "hotel:hotel"
superbHostel :: IRI
superbHostel = ID "hotel:superbHostel"
luxuryHotel :: IRI
luxuryHotel = ID "hotel:luxuryHotel"
restaurant :: IRI
restaurant = ID "hotel:restaurant"

dorm :: IRI
dorm = ID "hotel:dormitory"
doubleRoom :: IRI
doubleRoom = ID "hotel:doubleRoom"
suite :: IRI
suite = ID "hotel:suite"

breakfast :: IRI
breakfast = ID "hotel:beakfast"
smalltown :: IRI
smalltown = ID "hotel:smalltown"

hasRoomType :: IRI
hasRoomType = ID "hotel:hasRoomType"
servesMeal :: IRI
servesMeal = ID "hotel:servesMeal"
hasTakeAway :: IRI
hasTakeAway = ID "hotel:hasTakeAway"
hasKitchen :: IRI
hasKitchen = ID "hotel:hasKitchen"
locatedIn :: IRI
locatedIn = ID "hotel:locatedIn"

no :: IRI
no = ID "hotel:no"
yes :: IRI
yes = ID "hotel:yes"

lodgingRoom :: IRI
lodgingRoom = ID "_lodgingRoom"
lodgingRoomProperty :: Prototype IRI
lodgingRoomProperty =
  generatePropertyPrototype PP {pId = lodgingRoom
                              , propName = hasRoomType
                              , values = Set.empty
                              , typeConstraints = Set.empty
                              , cardinalityConstraints = Set.singleton atLeast1}

lodgingRoom2 :: IRI
lodgingRoom2 = ID "_lodgingRoom2"

lodgingRoomPropertyWithVal :: Prototype IRI
lodgingRoomPropertyWithVal =
  generatePropertyPrototype PP {pId = lodgingRoom2
                              , propName = hasRoomType
                              , values = Set.singleton suite
                              , typeConstraints = Set.empty
                              , cardinalityConstraints = Set.empty}

lodgingRoom3 :: IRI
lodgingRoom3 = ID "_lodgingRoom3"

lodgingRoomPropertyWithVal2 :: Prototype IRI
lodgingRoomPropertyWithVal2 =
  generatePropertyPrototype PP {pId = lodgingRoom3
                              , propName = hasRoomType
                              , values = Set.singleton suite
                              , typeConstraints = Set.empty
                              , cardinalityConstraints = Set.empty}


atLeast1 :: IRI
atLeast1 = ID "_atLeast1"

atLeast1ConstraintInfo :: ConstraintInfo
atLeast1ConstraintInfo = generateCardConstraintLower 1

atLeast1Constraint :: Prototype IRI
atLeast1Constraint = convertConstraintInfoToProto atLeast1 atLeast1ConstraintInfo

hotelKB :: FixpointKnowledgeBase IRI
hotelKB = Map.fromList [(lodgingRoom, lodgingRoomProperty)
  , (atLeast1, atLeast1Constraint)
  , (lodgingRoom2, lodgingRoomPropertyWithVal)
  , (lodgingRoom3, lodgingRoomPropertyWithVal2)
  ]
