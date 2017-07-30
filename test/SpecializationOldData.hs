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

module SpecializationOldData where

import Prototype.Basis
import qualified Prototype.SpecializationOld as Special
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import TestData

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
