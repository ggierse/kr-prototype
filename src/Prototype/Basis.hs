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

module Prototype.Basis where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List

type Set = Set.Set

data IRI = ID String deriving (Show, Eq, Ord)
data Property = Prop IRI deriving (Show, Eq, Ord)
type PropertyMap = Map.Map Property (Set.Set IRI)
data Bases = Base IRI | P0 deriving (Show, Eq)

data SimpleChangeExpression = Change Property (Set.Set IRI) deriving (Show, Eq, Ord)

data PrototypeExpression = Proto {
  base :: Bases,
  add :: Set.Set SimpleChangeExpression,
  remove :: Set.Set SimpleChangeExpression} deriving (Show, Eq)

data Prototype = PT {name :: IRI, properties :: PropertyMap} deriving (Show, Eq)

type KnowledgeBase = Map.Map IRI PrototypeExpression




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

isFixPoint :: PrototypeExpression -> Bool
isFixPoint Proto {base=P0, add=_, remove = rem1}
  | Set.null rem1 = True
  | otherwise = False
isFixPoint _ = False

computeAllFixpoints :: KnowledgeBase -> KnowledgeBase
computeAllFixpoints kb = Map.mapWithKey (\ key _ -> computeFixpoint kb key) kb


computeFixpoint :: KnowledgeBase -> IRI -> PrototypeExpression
computeFixpoint kbMap iri =
  let original = kbMap Map.! iri
      branch = getBranchToP0 kbMap original
      prototype = branchToPrototype iri branch
  in prototypeToFixpoint prototype

prototypeToFixpoint :: Prototype -> PrototypeExpression
prototypeToFixpoint PT {name=_iri, properties=props} =
  Proto{base=P0, add=propertyMapToChangeExpressions props, remove=Set.empty}

propertyMapToChangeExpressions :: PropertyMap -> Set.Set SimpleChangeExpression
propertyMapToChangeExpressions = Map.foldlWithKey foldPropertyIrisToChangeSet Set.empty

foldPropertyIrisToChangeSet :: Set.Set SimpleChangeExpression -> Property -> Set.Set IRI -> Set.Set SimpleChangeExpression
foldPropertyIrisToChangeSet changeSet propName iris =
  Set.insert (Change propName iris) changeSet

branchToPrototype :: IRI -> [PrototypeExpression] -> Prototype
branchToPrototype iri branch =
  let basePrototype = PT {name=iri, properties=Map.empty}
   in List.foldr applyPrototypeExpression basePrototype branch

-- TODO more than one simple change expression
applyPrototypeExpression :: PrototypeExpression -> Prototype -> Prototype
applyPrototypeExpression
  Proto{base=_, add=add1, remove=rem1} PT{name=iri, properties=plist} =
    PT{name=iri, properties=addProperties (removeProperties plist rem1) add1}

removeProperties :: PropertyMap -> Set.Set SimpleChangeExpression -> PropertyMap
removeProperties = Set.foldl removeProperty

removeProperty :: PropertyMap -> SimpleChangeExpression -> PropertyMap
removeProperty propMap change =
  --filterWithKey (\prop _ -> prop /= properties)
  Map.filter (not . Set.null) (removeIfPropertyExists propMap change)
  --mapWithKey (\k v -> removeIrisIfPropertyEqual (k,v) (prop, iris) ) propMap

removeIfPropertyExists :: PropertyMap -> SimpleChangeExpression -> PropertyMap
removeIfPropertyExists propMap (Change prop iris) =
  Map.mapWithKey (\k v -> removeIrisIfPropertyEqual (k,v) (prop, iris) ) propMap

removeIrisIfPropertyEqual :: (Property, Set.Set IRI) -> (Property, Set.Set IRI) -> Set.Set IRI
removeIrisIfPropertyEqual (pBase, irisBase) (pRemove,irisRemove)
  | pBase == pRemove = irisBase Set.\\ irisRemove
  | otherwise = irisBase

addProperties :: PropertyMap -> Set.Set SimpleChangeExpression -> PropertyMap
addProperties = Set.foldl addProperty

addProperty :: PropertyMap -> SimpleChangeExpression -> PropertyMap
addProperty propMap (Change prop iris) =
  let maybeIris = Map.lookup prop propMap in
    case maybeIris of
      Just oldIris ->  Map.insert prop (Set.union oldIris iris) propMap
      Nothing -> Map.insert prop iris propMap


getBranchToP0 :: KnowledgeBase -> PrototypeExpression -> [PrototypeExpression]
getBranchToP0 kbMap proto@Proto{base=parent, add=_, remove=_} =
  let parentMaybeIri = baseToIri parent
  in case parentMaybeIri of
    Just parentIri -> proto : getBranchToP0 kbMap (kbMap Map.! parentIri)
    Nothing -> [proto]
