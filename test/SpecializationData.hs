-----------------------------------------------------------------------------
--
-- Module    :  Prototype.Basis
-- Copyright (C) 2017 Gesche Gierse
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
-----------------------------------------------------------------------------

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
hotel2 :: IRI
hotel2 = ID "hotel:hotel"
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

lodgingPrototype :: Prototype IRI
lodgingPrototype = PT {name=lodging, props=generateHasProperty $ Set.fromList [lodgingRoom]}

hotelPrototype :: Prototype IRI
hotelPrototype = PT {name=hotel, props=generateHasProperty $ Set.fromList [lodgingRoom2]}

hotelPrototype2 :: Prototype IRI
hotelPrototype2 = PT {name=hotel, props=generateHasProperty $ Set.fromList [lodgingRoom3]}



hotelKB :: FixpointKnowledgeBase IRI
hotelKB = Map.fromList [(lodgingRoom, lodgingRoomProperty)
  , (atLeast1, atLeast1Constraint)
  , (lodgingRoom2, lodgingRoomPropertyWithVal)
  , (lodgingRoom3, lodgingRoomPropertyWithVal2)
  , (lodging, lodgingPrototype)
  , (hotel2, hotelPrototype2)
  , (hotel, hotelPrototype)
  ]
