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
    getBranchToP0,
    IRI(..),
    Property(..),
    Bases(..),
    SimpleChangeExpression(..),
    PrototypeExpression(..),
    KnowledgeBase(..),
) where

import Data.Map.Strict as Map

data IRI = ID String deriving (Show, Eq, Ord)
data Property = Prop IRI deriving (Show, Eq)
data PropertyList = PList (Map Property [IRI]) deriving (Show, Eq)
data Bases = Base IRI | P0 deriving (Show, Eq)

data SimpleChangeExpression = Change Property [IRI] deriving (Show, Eq)

data PrototypeExpression = Proto {base :: Bases, add :: [SimpleChangeExpression], remove :: [SimpleChangeExpression]} deriving (Show, Eq)

data Prototype = PT {iri :: IRI, properties :: PropertyList}

data KnowledgeBase = KB (Map IRI PrototypeExpression)




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
computeFixpoint kb@(KB kbMap) iri =
  let original = kbMap ! iri
      branch = getBranchToP0 kb original
  in original
-- TODO finish this

getBranchToP0 :: KnowledgeBase -> PrototypeExpression -> [PrototypeExpression]
getBranchToP0 kb@(KB kbMap) proto@Proto{base=parent, add=_, remove=_} =
  let parentMaybeIri = baseToIri parent
  in case parentMaybeIri of
    Just parentIri -> proto : getBranchToP0 kb (kbMap ! parentIri)
    Nothing -> [proto]

-- computeFixpointFromBranch :: [PrototypeExpression] -> PrototypeExpression
--computeFixpointFromBranch list =
--  foldr f list

--applyPrototypeChainOnce :: PrototypeExpression -> PrototypeExpression -> PrototypeExpression
--applyPrototypeChainOnce Proto{base=b1, add=a1, remove=r1} Proto{base=P0, add=a2, remove=r2} =
--  Proto{base=P0, }
