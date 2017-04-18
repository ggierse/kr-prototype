module Prototype.Specialization where

import Prototype.Basis
import Prototype.Composed

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Debug.Trace
import Data.Maybe (isJust, fromJust)

{--
Specialization relation
given prototypes s,g: s isSpecializationOf g if for all prototypes G \in properties(g) it holds that
there exists a prototype S \in properties(s) such that:
  G.id = S.id
  and S isSpecializationOf G.
--}


isSpecializationOf :: FixpointKnowledgeBase IRI -> Prototype IRI -> Prototype IRI -> Bool
isSpecializationOf fkb special general =
    forAllGeneralExistsSpecial fkb sprops gprops
    where gprops = properties fkb general
          sprops = properties fkb special

forAllGeneralExistsSpecial fkb sprops =
  Set.foldl' (\ prev g -> prev && existsSpecial fkb sprops g) True

existsSpecial fkb sprops g =
  case samePropertyName of
    Nothing -> False
    Just s -> isPropertySpecialization fkb s g
  where samePropertyName = List.find (propertyIdIsEqual g) $ Set.toList sprops

propertyIdIsEqual :: Prototype IRI -> Prototype IRI -> Bool
propertyIdIsEqual a b =
  accessFirstOfProperty a hasID == accessFirstOfProperty b hasID

isPropertySpecialization :: FixpointKnowledgeBase IRI -> Prototype IRI -> Prototype IRI -> Bool
isPropertySpecialization fkb s g
  | Set.null gConsts = val g `Set.isSubsetOf` val s && accountFor s gConsts
  | otherwise = True
  where gConsts = consts fkb g

accountFor :: Prototype IRI -> Set ConstraintInfo -> Bool
accountFor s gConsts= False
