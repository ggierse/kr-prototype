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

data IRI = ID String deriving (Show)
instance Eq IRI where
    (ID s1) == (ID s2) = s1 == s2
instance Ord IRI where
    (ID s1) `compare` (ID s2) = s1 `compare` s2

data Property = Prop IRI deriving (Show)
instance Eq Property where
    Prop iri1 == Prop iri2 = iri1 == iri2

data Bases = Base IRI | P0 deriving (Show)
instance Eq Bases where
    P0 == P0 = True
    Base b1 == Base b2 = b1 == b2
    _ == _ = False

data SimpleChangeExpression = Change Property [IRI] | Empty deriving (Show)
instance Eq SimpleChangeExpression where
    Empty == Empty = True
    Change prop1 list1 == Change prop2 list2 = prop1 == prop2 && list1 == list2
    _ == _ = False

data PrototypeExpression = Proto {base :: Bases, add :: SimpleChangeExpression, remove :: SimpleChangeExpression} deriving (Show)
instance Eq PrototypeExpression where
    Proto {base=b1, add=a1, remove=r1} == Proto {base=b2, add=a2, remove=r2} = b1 == b2 && a1 == a2 && r1 == r2

data KnowledgeBase = KB (Map IRI PrototypeExpression)




-- |
-- Convert IRI to Bases type
--
-- >>> iriToBase (ID "test:test")
-- Base (ID "test:test")
iriToBase :: IRI -> Bases
iriToBase = Base

isFixPoint :: PrototypeExpression -> Bool
isFixPoint Proto {base=P0, add=_, remove = Empty} = True
isFixPoint _ = False

computeFixpoint :: KnowledgeBase -> IRI -> PrototypeExpression
computeFixpoint _ _ = Proto {base=P0, add=Empty, remove=Empty}
