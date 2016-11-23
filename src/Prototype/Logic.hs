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

isFixPoint :: PrototypeExpression -> Bool
isFixPoint Proto {base=P0, add=_, remove = []} = True
isFixPoint _ = False

computeFixpoint :: KnowledgeBase -> IRI -> PrototypeExpression
computeFixpoint _ _ = Proto {base=P0, add=Empty, remove=Empty}
