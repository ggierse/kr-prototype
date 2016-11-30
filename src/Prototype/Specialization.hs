module Prototype.Specialization where

import Prototype.Basis

import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust)


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

getIris :: Set.Set ComplexValue -> Set.Set IRI
getIris complexSet =
  let maybeIris = Set.map getIRI complexSet
      justIris = Set.filter isJust maybeIris
  in Set.map fromJust justIris

instance (Ord a) => Specializable (ChangeExpression a) (ChangeExpression a) where
  isSpecializationOf special@(Change propS propSetS) general@(Change propG propSetG)
    | propS /= propG = False
    | general == special = True
    | propSetS `Set.isSubsetOf` propSetG = True
    | otherwise = False

    {-- für alle properties der generalization muss gelten
     ach warte das ist komisch für nur eine change expression.
     Was machen wir überhaupt, wenn wir ein (atLeast 5) und irgendwelche iris haben?
     -> könnte man interpretieren als atleast 5 und diese hier sind fix
--}
