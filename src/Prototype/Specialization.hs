module Prototype.Specialization where

import Prototype.Basis

import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust)
import Debug.Trace

data Constraint = Exactly Int | Atleast Int | Atmost Int deriving (Show, Eq, Ord)
data ComplexValue = Value IRI | Const Constraint deriving (Show, Eq, Ord)


protoDefIriToComplex :: PrototypeDefinition IRI -> PrototypeDefinition ComplexValue
protoDefIriToComplex Proto {base=b, add=a, remove=r, remAll=r2} =
  Proto{base=b, add=simpleChangeSetToComplex a, remove=simpleChangeSetToComplex r, remAll=r2}

simpleChangeSetToComplex :: Set.Set SimpleChangeExpression -> Set.Set (ChangeExpression ComplexValue)
simpleChangeSetToComplex = Set.map simpleChangeToComplex

simpleChangeToComplex :: SimpleChangeExpression -> ChangeExpression ComplexValue
simpleChangeToComplex (Change prop values) = Change prop (Set.map Value values)

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
--class Specializable a where
--  isSpecializationOf :: a -> a -> Bool

class Specializable a b where
  isSpecializationOf :: a -> b -> Bool

instance Specializable Constraint Constraint where
  isSpecializationOf (Exactly s) (Exactly g) = s == g
  isSpecializationOf (Atleast s) (Atleast g) = s >= g
  isSpecializationOf (Atmost s) (Atmost g) = s <= g
  isSpecializationOf _ _ = False

instance Specializable ComplexValue ComplexValue where
  isSpecializationOf (Value s) (Value g) = s == g
  isSpecializationOf (Const s) (Const g) = s `isSpecializationOf` g
  isSpecializationOf _ _ = False

instance Specializable (Set.Set IRI) Constraint where
  isSpecializationOf set (Exactly n) = Set.size set == n
  isSpecializationOf set (Atmost n) = Set.size set <= n
  isSpecializationOf set (Atleast n) = Set.size set >= n

instance Specializable (Set.Set ComplexValue) Constraint where
  isSpecializationOf set g =
    let iri = getIris set `isSpecializationOf` g -- problem: in case that constraint - constraint, we cannot fullfil this here
        consts = getConstraints set `isSpecializationOf` g
    in consts `debug` ("const condition: " ++ show consts) || iri `debug` ("iri condition: " ++ show iri)

instance Specializable (Set.Set Constraint) Constraint where
  isSpecializationOf set g
    | Set.size set == 0 = False
    | otherwise = Set.foldl (\ prev s -> prev && s `isSpecializationOf` g) True set `debug` "going here"

instance Specializable (Set.Set IRI) (Set.Set IRI) where
  isSpecializationOf sSet gSet = gSet `Set.isSubsetOf` sSet

instance Specializable (Set.Set ComplexValue) (Set.Set ComplexValue) where
  isSpecializationOf sSet gSet =
    let res = Set.foldl (compareSpecs sSet) True (getConstraints gSet)
    in res
    `debug` ("fold in complex results in "++show res)
    && getIris sSet `isSpecializationOf` getIris gSet
    --Set.foldl (\ prev g -> prev && (getIris sSet) `isSpecializationOf` g) True (getIris gSet)
  -- für alle number constraints muss es erfüllt sein (iris), kann aber auch nur weitere einschränkung geben
  -- für alle iris muss es genaue entsprechung geben
compareSpecs :: Set.Set ComplexValue -> Bool -> Constraint -> Bool
compareSpecs sSet prev g = (prev && (sSet `isSpecializationOf` g)) `debug` ("comparing " ++ show sSet ++ " with " ++ show g)


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

instance Specializable SimpleChangeExpression SimpleChangeExpression where
  isSpecializationOf special@(Change propS propSetS) general@(Change propG propSetG)
    | propS /= propG = False
    | general == special = True
    | propSetS `Set.isSubsetOf` propSetG = True
    | otherwise = False

instance Specializable (ChangeExpression ComplexValue) (ChangeExpression ComplexValue) where
  isSpecializationOf special@(Change propS propSetS) general@(Change propG propSetG)
    | propS /= propG = False `debug` "not the same property"
    | general == special = True `debug` "exactly the same change set"
    | propSetS `Set.isSubsetOf` propSetG = True `debug` "subset"
    | propSetS `isSpecializationOf` propSetG = True `debug` "specialization"
    | otherwise = False `debug` ("otherwise case, propSetS: " ++ show propSetS ++ " propSetG: " ++ show propSetG)

debug = flip trace

evaluateConstraints :: Set.Set Constraint -> Set.Set IRI -> Bool
{--
evaluateConstraints :: Set.Set ComplexValue -> Set.Set IRI -> Bool
evaluateConstraints constraints iris =
  Set.foldl (\ prev x -> prev && (iris `isSpecializationOf` x)) True constraints
  --}

    {-- für alle properties der generalization muss gelten
     ach warte das ist komisch für nur eine change expression.
     Was machen wir überhaupt, wenn wir ein (atLeast 5) und irgendwelche iris haben?
     -> könnte man interpretieren als atleast 5 und diese hier sind fix
--}
