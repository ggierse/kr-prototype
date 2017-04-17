module Prototype.Specialization where

import Prototype.Basis

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust, mapMaybe, fromMaybe)
import Debug.Trace
import Data.IntegerInterval
import Text.Read
--import qualified Data.IntegerInterval as Interval
--import Data.ExtendedReal (Extended)

data Constraint = Exactly Int | Atleast Int | Atmost Int deriving (Show, Eq, Ord)
data ComplexValue = Value IRI | Const Constraint deriving (Show, Eq, Ord)

data ConstraintName = AllValuesFrom | SomeValuesFrom | Cardinality deriving (Show, Eq, Ord)



data ConstraintInfo =  TypeConst {
  constType :: ConstraintName,
  constValues :: Set IRI
} | CardinalityConst {
  constType :: ConstraintName,
  constInterval :: IntegerInterval
} deriving (Show, Eq, Ord)

-- TODO is ordering like this ok? does this interfere with how things
-- are converted to/from lists? does anything get lost if it is equal?
{--instance Ord ConstraintInfo where
  TypeConst _ _ `compare` CardinalityConst _ _ = GT
  CardinalityConst _ _ `compare` TypeConst _ _ = LT
  --TypeConst name1 _ `compare` TypeConst name2 _ = name1 `compare` name2
  --CardinalityConst name1 _ `compare` CardinalityConst name2 _ = name1 `compare` name2
  a `compare` b = constType a `compare` constType b
--}

instance Ord IntegerInterval where
  a `compare` b = (lowerBound a, upperBound a) `compare` (lowerBound b, upperBound b)



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

-- Composed prototypes
hasProperty :: Property
hasProperty = Prop (ID "proto:hasProperty")

accessProperty :: Prototype IRI -> Property -> Set IRI
accessProperty proto property = fromMaybe Set.empty value
  where value = Map.lookup property $ props proto

properties :: FixpointKnowledgeBase IRI -> Prototype IRI -> Set (Prototype IRI)
properties fkb proto = Set.map (\ iri -> fkb Map.! iri) (accessProperty proto hasProperty)

-- Property prototypes
hasID :: Property
hasID = Prop (ID "proto:hasID")

hasValue :: Property
hasValue = Prop (ID "proto:hasValue")

hasTypeConstraint :: Property
hasTypeConstraint = Prop (ID "proto:hasTypeConstraint")

hasCardinalityConstraint :: Property
hasCardinalityConstraint = Prop (ID "proto:hasCardinalityConstraint")

val :: Prototype IRI -> Set IRI
val proto = accessProperty proto hasValue

isTypeConstraintPrototype :: Prototype IRI -> Bool
isTypeConstraintPrototype proto =
  (&&) (Set.size (accessProperty proto hasConstraintType) == 1)
       ( (&&) (not $ Set.null (accessProperty proto hasConstraintValue))
             (isJust $ convertIriToConstName $ Set.elemAt 0 $ accessProperty proto hasConstraintType))


convertTypeConstProto :: Prototype IRI -> Maybe ConstraintInfo
convertTypeConstProto proto
  | defined = Just TypeConst {constType=ctype,  constValues=cvals}
  | otherwise = Nothing
  where defined = isTypeConstraintPrototype proto
        ctype = fromJust $ convertIriToConstName $ Set.elemAt 0 $ accessProperty proto hasConstraintType
        cvals = accessProperty proto hasConstraintValue


consts :: FixpointKnowledgeBase IRI -> Prototype IRI -> Set ConstraintInfo
consts fkb proto = Set.fromList $ mapMaybe convertTypeConstProto cTypeProtos ++ mapMaybe convertCardConstProto cCardProtos
  where typeConstProtos = accessProperty proto hasTypeConstraint
        cardinalityConstProtos = accessProperty proto hasCardinalityConstraint
        cTypeProtos = Set.toList $ Set.map (\ iri -> fkb Map.! iri) typeConstProtos
        cCardProtos = Set.toList $ Set.map (\ iri -> fkb Map.! iri) cardinalityConstProtos

convertCardConstProto :: Prototype IRI -> Maybe ConstraintInfo
convertCardConstProto proto
  | defined = Just CardinalityConst {constType=Cardinality, constInterval=cardint}
  | otherwise = Nothing
  where defined = isCardConstraintPrototype proto
        cardint = Data.IntegerInterval.empty

isCardConstraintPrototype :: Prototype IRI -> Bool
isCardConstraintPrototype proto =
  (Set.size lowerVals == 1) &&
  (Set.size upperVals == 1) &&
  isJust ( convertIriToInteger $ Set.elemAt 0 lowerVals) &&
  isJust ( convertIriToExtendedInteger $ Set.elemAt 0 upperVals)
  where lowerVals = accessProperty proto lower
        upperVals = accessProperty proto upper

convertIriToInteger :: IRI -> Maybe Integer
convertIriToInteger (ID str) = readMaybe str

convertIriToExtendedInteger :: IRI -> Maybe (Extended Integer)
convertIriToExtendedInteger iri@(ID str)
  | iri == infty = Just PosInf
  | otherwise = case readMaybe str of
      Just int -> Just (Finite int)
      Nothing -> Nothing

-- Type/Cardinality Constraint Prototypes
hasConstraintValue :: Property
hasConstraintValue = Prop $ ID "proto:hasConstraintValue"

hasConstraintType :: Property
hasConstraintType = Prop $ ID "proto:hasConstraintType"

lower :: Property
lower = Prop $ ID "proto:lower"

upper :: Property
upper = Prop $ ID "proto:upper"

infty :: IRI
infty = ID "proto:infty"

allValuesFrom :: IRI
allValuesFrom = ID "proto:allValuesFrom"

someValuesFrom :: IRI
someValuesFrom = ID "proto:someValuesFrom"

convertIriToConstName :: IRI -> Maybe ConstraintName
convertIriToConstName iri
  | iri == allValuesFrom = Just AllValuesFrom
  | iri == someValuesFrom = Just SomeValuesFrom
  | otherwise = Nothing

{--
Specialization relation
given prototypes s,g: s isSpecializationOf g if for all prototypes G \in properties(g) it holds that
there exists a prototype S \in properties(s) such that:
  G.id = S.id
  and S isSpecializationOf G.
--}


class Specializable a b where
  isSpecializationOf :: a -> b -> Bool


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
