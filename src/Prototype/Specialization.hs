module Prototype.Specialization where

import Prototype.Basis
import Prototype.Composed

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust)
import Data.IntegerInterval as Interval


{--
Specialization relation
given prototypes s,g: s isSpecializationOf g if for all prototypes G in properties(g) it holds that
there exists a prototype S in properties(s) such that:
  G.id = S.id
  and S isSpecializationOf G.
--}


isSpecializationOf :: FixpointKnowledgeBase IRI -> Prototype IRI -> Prototype IRI -> Bool
isSpecializationOf fkb special general =
    forall existsSpecial gprops
    where gprops = properties fkb general
          sprops = properties fkb special
          existsSpecial g = exists (\s -> propertyIdIsEqual s g && isPropertySpecialization fkb s g) sprops

propertyIdIsEqual :: Prototype IRI -> Prototype IRI -> Bool
propertyIdIsEqual a b =
  accessFirstOfProperty a hasID == accessFirstOfProperty b hasID

isPropertySpecialization :: FixpointKnowledgeBase IRI -> Prototype IRI -> Prototype IRI -> Bool
isPropertySpecialization fkb s g
  | not (Set.null gConsts) = val g `Set.isSubsetOf` val s && accountFor fkb s gConsts
  | otherwise = isPropertyProtoEqual s g
  where gConsts = consts fkb g

isPropertyProtoEqual :: Prototype IRI -> Prototype IRI -> Bool
isPropertyProtoEqual a b =
  propsEq a b hasID &&
  propsEq a b hasValue &&
  propsEq a b hasTypeConstraint &&
  propsEq a b hasCardinalityConstraint
  where getProp proto prop = props proto Map.! prop
        propsEq p1 p2 prop = getProp p1 prop == getProp p2 prop


accountFor :: FixpointKnowledgeBase IRI -> Prototype IRI -> Set ConstraintInfo -> Bool
accountFor fkb s gConsts
  | Set.null sConsts = forall (isSatisfied $ val s) gConsts
  | otherwise = forall (isMatched sConsts) gConsts
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
    AllValuesFrom -> forall (\ v -> v `Set.member` constValues gc) sVals
    SomeValuesFrom -> exists (\ v -> v `Set.member` constValues gc) sVals
    Cardinality -> toInteger (Set.size sVals) `Interval.member` constInterval gc


exists :: Foldable t =>  (t1 -> Bool) -> t t1 -> Bool
exists condition foldable = isJust $ List.find condition foldable

forall :: (b -> Bool) -> Set b -> Bool
forall condition = Set.foldl' (\ prev v -> prev && condition v) True
