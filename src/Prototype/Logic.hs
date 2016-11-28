-----------------------------------------------------------------------------
--
-- Module      :  Prototype.Logic
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

module Prototype.Logic (
    isFixPoint,
    iriToBase,
    computeFixpoint,
    branchToPrototype,
    getBranchToP0,
    removeProperty,
    removeProperties,
    applyPrototypeExpression,
    IRI(..),
    Property(..),
    PropertyMap,
    Bases(..),
    SimpleChangeExpression(..),
    PrototypeExpression(..),
    Prototype(..),
    KnowledgeBase,
) where

import Data.Map.Strict as Map
import Data.List as List

data IRI = ID String deriving (Show, Eq, Ord)
data Property = Prop IRI deriving (Show, Eq, Ord)
type PropertyMap = Map Property [IRI]
data Bases = Base IRI | P0 deriving (Show, Eq)

data SimpleChangeExpression = Change Property [IRI] deriving (Show, Eq)

data PrototypeExpression = Proto {base :: Bases, add :: [SimpleChangeExpression], remove :: [SimpleChangeExpression]} deriving (Show, Eq)

data Prototype = PT {name :: IRI, properties :: PropertyMap} deriving (Show, Eq)

type KnowledgeBase = Map IRI PrototypeExpression




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

--protoexpToPrototype :: PrototypeExpression -> IRI -> Prototype
--protoexpToPrototype Proto{base=P0, add=Change property valueList, remove=r1} iri =
--  PT{iri=iri, properties=[]}

isFixPoint :: PrototypeExpression -> Bool
isFixPoint Proto {base=P0, add=_, remove = []} = True
isFixPoint _ = False

computeFixpoint :: KnowledgeBase -> IRI -> PrototypeExpression
computeFixpoint kbMap iri =
  let original = kbMap ! iri
      --branch = getBranchToP0 kbMap original
  in original --PT {name=iri, properties=PList empty}
--}
-- TODO finish this

branchToPrototype :: IRI -> [PrototypeExpression] -> Prototype
branchToPrototype iri []= PT {name=iri, properties=empty}
branchToPrototype iri (_head : _tail) = PT{name=iri, properties=empty}

-- TODO more than one simple change expression
applyPrototypeExpression :: Prototype -> PrototypeExpression -> Prototype
applyPrototypeExpression
  PT{name=iri, properties=plist} Proto{base=_, add=_add1, remove=[rem1]} =
    PT{name=iri, properties=removeProperty plist rem1}
applyPrototypeExpression (PT _ _) Proto{} = PT{name=ID "", properties=empty}
    --addProperty (removeProperty plist rem1) add1

removeProperties :: PropertyMap -> [SimpleChangeExpression] -> PropertyMap
removeProperties _ _ = empty

removeProperty :: PropertyMap -> SimpleChangeExpression -> PropertyMap
removeProperty propMap change =
  --filterWithKey (\prop _ -> prop /= properties)
  Map.filter (not . List.null) (removeIfPropertyExists propMap change)
  --mapWithKey (\k v -> removeIrisIfPropertyEqual (k,v) (prop, iris) ) propMap

removeIfPropertyExists :: Map Property [IRI] -> SimpleChangeExpression -> Map Property [IRI]
removeIfPropertyExists propMap (Change props iris) =
  mapWithKey (\k v -> removeIrisIfPropertyEqual (k,v) (props, iris) ) propMap

removeIrisIfPropertyEqual :: (Property, [IRI]) -> (Property, [IRI]) -> [IRI]
removeIrisIfPropertyEqual (pBase, irisBase) (pRemove,irisRemove)
  | pBase == pRemove = irisBase List.\\ irisRemove
  | otherwise = irisBase


{-- addProperty :: PropertyMap -> SimpleChangeExpression -> PropertyMap
addProperty PList mapping Change prop iris =
  addIrisIfPropertyEqual
--}
  -- wenn die property übereinstimmt muss es zu der iri liste hinzugefügt werden
  -- sonst neuer eintrag



getBranchToP0 :: KnowledgeBase -> PrototypeExpression -> [PrototypeExpression]
getBranchToP0 kbMap proto@Proto{base=parent, add=_, remove=_} =
  let parentMaybeIri = baseToIri parent
  in case parentMaybeIri of
    Just parentIri -> proto : getBranchToP0 kbMap (kbMap ! parentIri)
    Nothing -> [proto]

-- computeFixpointFromBranch :: [PrototypeExpression] -> PrototypeExpression
--computeFixpointFromBranch list =
--  foldr f list

--applyPrototypeChainOnce :: PrototypeExpression -> PrototypeExpression -> PrototypeExpression
--applyPrototypeChainOnce Proto{base=b1, add=a1, remove=r1} Proto{base=P0, add=a2, remove=r2} =
--  Proto{base=P0, }
