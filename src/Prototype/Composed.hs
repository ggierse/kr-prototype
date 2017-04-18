module Prototype.Composed where

import Prototype.Basis

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (isJust, fromJust, mapMaybe, fromMaybe)
import Data.IntegerInterval as Interval
import Text.Read

data ConstraintName = AllValuesFrom | SomeValuesFrom | Cardinality
  deriving (Show, Eq, Ord)

data ConstraintInfo =  TypeConst {
  constType :: ConstraintName,
  constValues :: Set IRI
} | CardinalityConst {
  constType :: ConstraintName,
  constInterval :: IntegerInterval
} deriving (Show, Eq, Ord)

instance Ord IntegerInterval where
  a `compare` b =
    (lowerBound a, upperBound a) `compare` (lowerBound b, upperBound b)

-- Composed prototypes
hasProperty :: Property
hasProperty = Prop (ID "proto:hasProperty")

accessProperty :: Prototype IRI -> Property -> Set IRI
accessProperty proto property = fromMaybe Set.empty value
  where value = Map.lookup property $ props proto

accessFirstOfProperty :: Prototype IRI -> Property -> IRI
accessFirstOfProperty proto property = Set.elemAt 0 $ accessProperty proto property

properties :: FixpointKnowledgeBase IRI -> Prototype IRI -> Set (Prototype IRI)
properties fkb proto =
  Set.map (\ iri -> fkb Map.! iri) (accessProperty proto hasProperty)

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



convertTypeConstProto :: Prototype IRI -> Maybe ConstraintInfo
convertTypeConstProto proto
  | defined = Just TypeConst {constType=ctype,  constValues=cvals}
  | otherwise = Nothing
  where defined = isTypeConstraintPrototype proto
        ctype = fromJust $ convertIriToConstName typeIri
        typeIri = accessFirstOfProperty proto hasConstraintType
        cvals = accessProperty proto hasConstraintValue

isTypeConstraintPrototype :: Prototype IRI -> Bool
isTypeConstraintPrototype proto =
  (&&) (Set.size (accessProperty proto hasConstraintType) == 1)
  ( (&&) (not $ Set.null (accessProperty proto hasConstraintValue))
  (isJust $ convertIriToConstName $ accessFirstOfProperty proto hasConstraintType))

consts :: FixpointKnowledgeBase IRI -> Prototype IRI -> Set ConstraintInfo
consts fkb proto =
  Set.fromList $
    mapMaybe convertTypeConstProto cTypeProtos ++
    mapMaybe convertCardConstProto cCardProtos
  where typeConstProtos = accessProperty proto hasTypeConstraint
        cardinalityConstProtos = accessProperty proto hasCardinalityConstraint
        cTypeProtos = Set.toList $ Set.map (\ iri -> fkb Map.! iri) typeConstProtos
        cCardProtos = Set.toList $ Set.map (\ iri -> fkb Map.! iri) cardinalityConstProtos

convertCardConstProto :: Prototype IRI -> Maybe ConstraintInfo
convertCardConstProto proto
  | defined = Just CardinalityConst {constType=Cardinality, constInterval=cardint}
  | otherwise = Nothing
  where defined = isCardConstraintPrototype proto
        cardint = parseInterval low up
        low = accessFirstOfProperty proto lower
        up = accessFirstOfProperty proto upper

isCardConstraintPrototype :: Prototype IRI -> Bool
isCardConstraintPrototype proto =
  (Set.size lowerVals == 1) &&
  (Set.size upperVals == 1) &&
  isJust ( convertIriToInteger $ Set.elemAt 0 lowerVals) &&
  isJust ( convertIriToExtendedInteger $ Set.elemAt 0 upperVals)
  where lowerVals = accessProperty proto lower
        upperVals = accessProperty proto upper

{-- this should only be used if isCardConstraintPrototype is True
 -- otherwise runtime exceptions might occur
 --}
parseInterval :: IRI -> IRI -> IntegerInterval
parseInterval l u = interval (Finite low, True) up
  where low = fromJust $ convertIriToInteger l
        up = fromJust $ convertIriToExtendedInteger u

convertIriToInteger :: IRI -> Maybe Integer
convertIriToInteger (ID str) = readMaybe str

convertIriToExtendedInteger :: IRI -> Maybe (Extended Integer, Bool)
convertIriToExtendedInteger iri@(ID str)
  | iri == infty = Just (PosInf, False)
  | otherwise = case readMaybe str of
      Just int -> Just (Finite int, True)
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
