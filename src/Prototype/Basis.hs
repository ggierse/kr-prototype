-----------------------------------------------------------------------------
--
-- Module      :  Prototype.Basis
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric #-}

module Prototype.Basis where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List
import GHC.Generics

type Set = Set.Set

data IRI = ID String deriving (Show, Eq, Ord, Generic)
data Property = Prop IRI deriving (Show, Eq, Ord, Generic)
type PropertyMap propValueType = Map.Map Property (Set.Set propValueType)
data Bases = Base IRI | P0 deriving (Show, Eq, Generic)

data ChangeExpression propValueType = Change Property (Set.Set propValueType) deriving (Show, Eq, Ord, Generic)
type SimpleChangeExpression = ChangeExpression IRI

data PrototypeExpression propValueType= Proto {
  base :: Bases,
  add :: Set.Set (ChangeExpression propValueType),
  remove :: Set.Set (ChangeExpression propValueType),
  remAll :: Set.Set Property} deriving (Show, Eq)

data Prototype propValueType = PT {name :: IRI, properties :: PropertyMap propValueType} deriving (Show, Eq)

type KnowledgeBase propValueType = Map.Map IRI (PrototypeExpression propValueType)


-- |
-- Convert IRI to Bases type
--
-- >>> iriToBase (ID "test:test")
-- Base (ID "test:test")
iriToBase :: IRI -> Bases
iriToBase = Base

baseToIri :: Bases -> Maybe IRI
baseToIri P0 = Nothing
baseToIri (Base iri) = Just iri

isFixPoint :: PrototypeExpression a -> Bool
isFixPoint Proto {base=P0, add=_, remove = rem1}
  | Set.null rem1 = True
  | otherwise = False
isFixPoint _ = False

computeAllFixpoints :: (Ord a) => KnowledgeBase a -> KnowledgeBase a
computeAllFixpoints kb = Map.mapWithKey (\ key _ -> computeFixpoint kb key) kb


computeFixpoint :: (Ord a) => KnowledgeBase a -> IRI -> PrototypeExpression a
computeFixpoint kbMap iri =
  let original = kbMap Map.! iri
      branch = getBranchToP0 kbMap original
      prototype = branchToPrototype iri branch
  in prototypeToFixpoint prototype

prototypeToFixpoint :: (Ord a) => Prototype a -> PrototypeExpression a
prototypeToFixpoint PT {name=_iri, properties=props} =
  Proto{base=P0, add=propertyMapToChangeExpressions props, remove=Set.empty}

propertyMapToChangeExpressions :: (Ord a) => PropertyMap a -> Set.Set (ChangeExpression a)
propertyMapToChangeExpressions = Map.foldlWithKey foldPropertyValuesToChangeSet Set.empty

foldPropertyValuesToChangeSet :: (Ord a) => Set.Set (ChangeExpression a) -> Property -> Set.Set a -> Set.Set (ChangeExpression a)
foldPropertyValuesToChangeSet changeSet propName iris =
  Set.insert (Change propName iris) changeSet

branchToPrototype :: (Ord a) => IRI -> [PrototypeExpression a] -> Prototype a
branchToPrototype iri branch =
  let basePrototype = PT {name=iri, properties=Map.empty}
   in List.foldr applyPrototypeExpression basePrototype branch

applyPrototypeExpression :: (Ord a) => PrototypeExpression a -> Prototype a -> Prototype a
applyPrototypeExpression
  Proto{base=_, add=add1, remove=rem1} PT{name=iri, properties=plist} =
    PT{name=iri, properties=addProperties (removeProperties plist rem1) add1}

removeProperties :: (Ord a) => PropertyMap a-> Set.Set (ChangeExpression a) -> PropertyMap a
removeProperties = Set.foldl removeProperty

removeProperty :: (Ord a) => PropertyMap a -> ChangeExpression a -> PropertyMap a
removeProperty propMap change =
  Map.filter (not . Set.null) (removeIfPropertyExists propMap change)

removeIfPropertyExists :: (Ord a) => PropertyMap a -> ChangeExpression a -> PropertyMap a
removeIfPropertyExists propMap (Change prop iris) =
  Map.mapWithKey (\k v -> removeValuesIfPropertyEqual (k,v) (prop, iris) ) propMap

removeValuesIfPropertyEqual :: (Ord a) => (Property, Set.Set a) -> (Property, Set.Set a) -> Set.Set a
removeValuesIfPropertyEqual (pBase, irisBase) (pRemove,irisRemove)
  | pBase == pRemove = irisBase Set.\\ irisRemove
  | otherwise = irisBase

addProperties :: (Ord a) => PropertyMap a -> Set.Set (ChangeExpression a) -> PropertyMap a
addProperties = Set.foldl addProperty

addProperty :: (Ord a) => PropertyMap a -> ChangeExpression a -> PropertyMap a
addProperty propMap (Change prop iris) =
  let maybeIris = Map.lookup prop propMap in
    case maybeIris of
      Just oldIris ->  Map.insert prop (Set.union oldIris iris) propMap
      Nothing -> Map.insert prop iris propMap


getBranchToP0 :: KnowledgeBase a -> PrototypeExpression a -> [PrototypeExpression a]
getBranchToP0 kbMap proto@Proto{base=parent, add=_, remove=_} =
  let parentMaybeIri = baseToIri parent
  in case parentMaybeIri of
    Just parentIri -> proto : getBranchToP0 kbMap (kbMap Map.! parentIri)
    Nothing -> [proto]
