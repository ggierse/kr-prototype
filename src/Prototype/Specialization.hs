module Prototype.Specialization where

import Prototype.Basis

import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe (isJust, fromJust)
import Debug.Trace

data Constraint = Exactly Int | Atleast Int | Atmost Int deriving (Show, Eq, Ord)
data ComplexValue = Value IRI | Const Constraint deriving (Show, Eq, Ord)

type ConstraintView = Maybe Constraint

class PossiblyConstraint a where
  isConstraint :: a -> Bool
  getConstraint :: a -> Maybe Constraint
  view :: a -> ConstraintView


instance PossiblyConstraint ComplexValue where
  isConstraint (Const _) = True
  isConstraint _ = False
  getConstraint (Const c) = Just c
  getConstraint _ = Nothing
  view = getConstraint

class PossiblyIRI a where
  getIRI :: a -> Maybe IRI

instance PossiblyIRI ComplexValue where
  getIRI (Value iri) = Just iri
  getIRI _ = Nothing


protoDefIriToComplex :: PrototypeExpression IRI -> PrototypeExpression ComplexValue
protoDefIriToComplex Proto {idIri=i, base=b, add=a, remove=r, remAll=r2} =
  Proto{idIri=i, base=b, add=simpleChangeSetToComplex a, remove=simpleChangeSetToComplex r, remAll=r2}

simpleChangeSetToComplex :: Set.Set SimpleChangeExpression -> Set.Set (ChangeExpression ComplexValue)
simpleChangeSetToComplex = Set.map simpleChangeToComplex

simpleChangeToComplex :: SimpleChangeExpression -> ChangeExpression ComplexValue
simpleChangeToComplex (Change prop values) = Change prop (Set.map Value values)

getChangeExpression :: Property -> [ComplexValue] -> ChangeExpression ComplexValue
getChangeExpression prop values = Change prop (Set.fromList values)



class Specializable a b where
  isSpecializationOf :: a -> b -> Bool


{--
Specialization relation
given prototypes s,g: s isSpecializationOf g if for all prototypes G \in properties(g) it holds that
there exists a prototype S \in properties(s) such that:
  G.id = S.id
  and S isSpecializationOf G.
--}

properties :: FixpointKnowledgeBase IRI -> Prototype a -> Set.Set (Prototype a)
properties fkb proto = Set.empty



instance Specializable (PrototypeExpression ComplexValue) (PrototypeExpression ComplexValue) where
  isSpecializationOf special@Proto {base=_, add=addS, remove=_, remAll=_} general@Proto {base=_, add=addG, remove=_, remAll=_}
    | not $ isFixPoint special = False
    | not $ isFixPoint general = False
    | otherwise = foldSpecialization addS addG--Set.foldr (\ g prev -> prev && (addS `isSpecializationOf` g)) True addG

foldSpecialization :: (Specializable a b) => a -> Set.Set b -> Bool
foldSpecialization special = Set.foldl' (\ prev g -> prev && (special `isSpecializationOf` g)) True

instance Specializable (Set.Set (ChangeExpression ComplexValue)) (ChangeExpression ComplexValue) where
  isSpecializationOf specials general =
    let sameName = List.find (changeExpressionNameIsEqual general) $ Set.toList specials
    in case sameName of
      Nothing -> False
      Just special -> special `isSpecializationOf` general

instance Specializable (ChangeExpression ComplexValue) (ChangeExpression ComplexValue) where
  isSpecializationOf special@(Change propS propSetS) general@(Change propG propSetG)
    | propS /= propG = False --`debug` "not the same property"
    | general == special = True --`debug` "exactly the same change set"
    | propSetG `Set.isSubsetOf` propSetS = True --`debug` "subset"
    | propSetS `isSpecializationOf` propSetG = True --`debug` "specialization"
    | otherwise = False --`debug` ("otherwise case, propSetS: " ++ show propSetS ++ " propSetG: " ++ show propSetG)

instance Specializable (Set.Set ComplexValue) (Set.Set ComplexValue) where
  isSpecializationOf sSet gSet =
    sSetIsSpecialOfEachgSet && getIris sSet `isSpecializationOf` getIris gSet
      where sSetIsSpecialOfEachgSet = foldSpecialization sSet (getConstraints gSet)

instance Specializable (Set.Set IRI) (Set.Set IRI) where
  isSpecializationOf sSet gSet = gSet `Set.isSubsetOf` sSet

instance Specializable (Set.Set ComplexValue) Constraint where
  isSpecializationOf set g =
    getConstraints set `isSpecializationOf` g || getIris set `isSpecializationOf` g

instance Specializable (Set.Set IRI) Constraint where
  isSpecializationOf set (Exactly n) = Set.size set == n
  isSpecializationOf set (Atmost n) = Set.size set <= n
  isSpecializationOf set (Atleast n) = Set.size set >= n

instance Specializable (Set.Set Constraint) Constraint where
  isSpecializationOf set g
    | Set.size set == 0 = False
    | otherwise = Set.foldl' (\ prev s -> prev && s `isSpecializationOf` g) True set

instance Specializable Constraint Constraint where
  isSpecializationOf (Exactly s) (Exactly g) = s == g
  isSpecializationOf (Atleast s) (Atleast g) = s >= g
  isSpecializationOf (Atmost s) (Atmost g) = s <= g
  isSpecializationOf _ _ = False

instance Specializable SimpleChangeExpression SimpleChangeExpression where
  isSpecializationOf special@(Change propS propSetS) general@(Change propG propSetG)
    | propS /= propG = False
    | general == special = True
    | propSetG `Set.isSubsetOf` propSetS = True
    | otherwise = False


{--instance Specializable ComplexValue ComplexValue where
  isSpecializationOf (Value s) (Value g) = s == g
  isSpecializationOf (Const s) (Const g) = s `isSpecializationOf` g
  isSpecializationOf _ _ = False

--}








getIris :: Set.Set ComplexValue -> Set.Set IRI
getIris complexSet =
  let maybeIris = Set.map getIRI complexSet
      justIris = Set.filter isJust maybeIris
  in Set.map fromJust justIris

getConstraints :: Set.Set ComplexValue -> Set.Set Constraint
getConstraints complexSet =
  let maybeConstraint = Set.map getConstraint complexSet
      justConstraint = Set.filter isJust maybeConstraint
  in Set.map fromJust justConstraint


debug :: c -> String -> c
debug = flip trace

-- TODO: check consistency with constraints (constraints are fullfilled in one change expression/within a prototype)
