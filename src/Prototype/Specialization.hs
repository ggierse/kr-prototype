module Prototype.Specialization where

import Prototype.Basis
import Prototype.Composed

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Debug.Trace
import Data.Maybe (isJust, fromJust)
import Data.IntegerInterval as Interval
{--
Specialization relation
given prototypes s,g: s isSpecializationOf g if for all prototypes G \in properties(g) it holds that
there exists a prototype S \in properties(s) such that:
  G.id = S.id
  and S isSpecializationOf G.
--}


isSpecializationOf :: FixpointKnowledgeBase IRI -> Prototype IRI -> Prototype IRI -> Bool
isSpecializationOf fkb special general =
    forall existsSpecial gprops
    --forAllGeneralExistsSpecial fkb sprops gprops
    where gprops = properties fkb general
          sprops = properties fkb special
          existsSpecial g = exists (\s -> propertyIdIsEqual s g && isPropertySpecialization fkb s g) sprops

{--
forAllGeneralExistsSpecial fkb sprops =
  forall existsSpecial -- Set.foldl' (\ prev g -> prev && existsSpecial g) True
  where existsSpecial g = exists (\s -> propertyIdIsEqual s g && isPropertySpecialization fkb s g) sprops
  --}

{--
existsSpecial fkb sprops g =
  case samePropertyName of
    Nothing -> False
    Just s -> isPropertySpecialization fkb s g
  where samePropertyName = List.find (propertyIdIsEqual g) sprops
  --}

propertyIdIsEqual :: Prototype IRI -> Prototype IRI -> Bool
propertyIdIsEqual a b =
  accessFirstOfProperty a hasID == accessFirstOfProperty b hasID

isPropertySpecialization :: FixpointKnowledgeBase IRI -> Prototype IRI -> Prototype IRI -> Bool
isPropertySpecialization fkb s g
  | Set.null gConsts = val g `Set.isSubsetOf` val s && accountFor fkb s gConsts
  | otherwise = True
  where gConsts = consts fkb g

accountFor :: FixpointKnowledgeBase IRI -> Prototype IRI -> Set ConstraintInfo -> Bool
accountFor fkb s gConsts
  | Set.null sConsts = forall (isSatisfied $ val s) gConsts --Set.foldl' (\ prev gc -> prev && isSatisfied (val s) gc) True gConsts
  | otherwise = forall (isMatched sConsts) gConsts --Set.foldl' (\ prev gc -> prev && isMatched sConsts gc) True gConsts
  where sConsts = consts fkb s


isMatched :: Foldable t => t ConstraintInfo -> ConstraintInfo -> Bool
isMatched sConsts gc =
  case gc of
    CardinalityConst _ _ ->
      exists (\sc -> constType sc == constType gc && constInterval sc `Interval.isSubsetOf` constInterval gc) sConsts
    TypeConst _ _ ->
      exists (\sc -> constType sc == constType gc && constValues sc `Set.isSubsetOf` constValues gc) sConsts


isSatisfied :: Set IRI -> ConstraintInfo -> Bool
isSatisfied sVals gc =
  case constType gc of
    AllValuesFrom -> forall (\ v -> v `Set.member` constValues gc) sVals --Set.foldl' (\ prev v -> prev && v `Set.member` constValues gc) True sVals
    SomeValuesFrom -> exists (\ v -> v `Set.member` constValues gc) sVals --isJust $ List.find (\ v -> v `Set.member` constValues gc) sVals
    Cardinality -> toInteger (Set.size sVals) `Interval.member` constInterval gc

exists :: Foldable t =>  (t1 -> Bool) -> t t1 -> Bool
exists condition foldable = isJust $ List.find condition foldable

forall :: (b -> Bool) -> Set b -> Bool
forall condition = Set.foldl' (\ prev v -> prev && condition v) True
