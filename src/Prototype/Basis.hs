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

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Either()
import Control.Exception as Exception
import Data.Typeable
import GHC.Generics


data IRI = ID String deriving (Show, Eq, Ord, Generic)
data Property = Prop IRI deriving (Show, Eq, Ord, Generic)
type PropertyMap propValueType = Map Property (Set propValueType)
data Bases = Base IRI | P0 deriving (Show, Eq, Ord, Generic)

data ChangeExpression propValueType = Change Property (Set propValueType) deriving (Show, Eq, Ord, Generic)
type SimpleChangeExpression = ChangeExpression IRI

data ConsistencyException = CycleDetected String | KeysInconsistent String |
                            BaseUndefined String | PropValUndefined String
                            deriving (Show, Typeable, Eq)

instance Exception ConsistencyException

{--
data PrototypeDefinition propValueType = Proto {
  base :: Bases,
  add :: Set (ChangeExpression propValueType),
  remove :: Set (ChangeExpression propValueType),
  remAll :: Set Property} deriving (Show, Eq)
  --}

data PrototypeExpression propValueType = Proto {
  idIri :: IRI,
  base :: Bases,
  add :: Set (ChangeExpression propValueType),
  remove :: Set (ChangeExpression propValueType),
  remAll :: Set Property} deriving (Show, Eq, Ord)

data Prototype propValueType = PT {name :: IRI, properties :: PropertyMap propValueType} deriving (Show, Eq)

type KnowledgeBase propValueType = Map IRI (PrototypeExpression propValueType)
showPretty :: (Show a) => KnowledgeBase a -> String
showPretty = Map.foldrWithKey foldMapEntryToStr ""

foldMapEntryToStr :: (Show a) => IRI -> a -> String -> String
foldMapEntryToStr key value prev = prev ++ show key ++ ": " ++ show value ++ "\n"

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

changeExpressionNameIsEqual :: ChangeExpression a -> ChangeExpression b -> Bool
changeExpressionNameIsEqual (Change nameA _) (Change nameB _) = nameA == nameB

isFixPoint :: PrototypeExpression a -> Bool
isFixPoint Proto {idIri=_, base=P0, add=_, remove = rem1, remAll = rem2}
  | Set.null rem1 && Set.null rem2 = True
  | otherwise = False
isFixPoint _ = False

computeAllFixpoints :: (Ord a) => KnowledgeBase a -> KnowledgeBase a
computeAllFixpoints kb = Map.mapWithKey (\ key _ -> computeFixpoint kb key) kb

computeAllFixpointsAsPrototypes :: (Ord a) => KnowledgeBase a -> [Prototype a]
computeAllFixpointsAsPrototypes kb = map (computeFixpointAsPrototype kb) (Map.keys kb)

computeFixpoint :: (Ord a) => KnowledgeBase a -> IRI -> PrototypeExpression a
computeFixpoint kbMap iri = prototypeToFixpoint $ computeFixpointAsPrototype kbMap iri

computeFixpointAsPrototype :: (Ord a) => KnowledgeBase a -> IRI -> Prototype a
computeFixpointAsPrototype kbMap iri =
  let original = kbMap Map.! iri
      branch = getBranchToP0 kbMap original
  in branchToPrototype iri branch

prototypeToFixpoint :: (Ord a) => Prototype a -> PrototypeExpression a
prototypeToFixpoint PT {name=iri, properties=props} =
  Proto{idIri=iri, base=P0, add=propertyMapToChangeExpressions props, remove=Set.empty, remAll=Set.empty}

propertyMapToChangeExpressions :: (Ord a) => PropertyMap a -> Set (ChangeExpression a)
propertyMapToChangeExpressions = Map.foldlWithKey foldPropertyValuesToChangeSet Set.empty

foldPropertyValuesToChangeSet :: (Ord a) => Set (ChangeExpression a) -> Property -> Set a -> Set (ChangeExpression a)
foldPropertyValuesToChangeSet changeSet propName iris =
  Set.insert (Change propName iris) changeSet

branchToPrototype :: (Ord a) => IRI -> [PrototypeExpression a] -> Prototype a
branchToPrototype iri branch =
  let basePrototype = PT {name=iri, properties=Map.empty}
   in List.foldr applyPrototypeExpression basePrototype branch

applyPrototypeExpression :: (Ord a) => PrototypeExpression a -> Prototype a -> Prototype a
applyPrototypeExpression
  Proto{base=_, add=add1, remove=rem1, remAll=rem2} PT{name=iri, properties=plist} =
    PT{name=iri, properties=addProperties (removeProperties (removeAllProps plist rem2) rem1) add1}

removeAllProps :: (Ord a) => PropertyMap a -> Set Property -> PropertyMap a
removeAllProps propMap propSet = Map.filterWithKey (\ k _ -> k `Set.notMember` propSet) propMap

removeProperties :: (Ord a) => PropertyMap a-> Set (ChangeExpression a) -> PropertyMap a
removeProperties = Set.foldl removeProperty

removeProperty :: (Ord a) => PropertyMap a -> ChangeExpression a -> PropertyMap a
removeProperty propMap change =
  Map.filter (not . Set.null) (removeIfPropertyExists propMap change)

removeIfPropertyExists :: (Ord a) => PropertyMap a -> ChangeExpression a -> PropertyMap a
removeIfPropertyExists propMap (Change prop iris) =
  Map.mapWithKey (\k v -> removeValuesIfPropertyEqual (k,v) (prop, iris) ) propMap

removeValuesIfPropertyEqual :: (Ord a) => (Property, Set a) -> (Property, Set a) -> Set a
removeValuesIfPropertyEqual (pBase, irisBase) (pRemove,irisRemove)
  | pBase == pRemove = irisBase Set.\\ irisRemove
  | otherwise = irisBase

addProperties :: (Ord a) => PropertyMap a -> Set (ChangeExpression a) -> PropertyMap a
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

generateKBfromPrototypeExps :: [PrototypeExpression a] -> KnowledgeBase a
generateKBfromPrototypeExps protos = Map.fromList $ map (\ p@(Proto ident _ _ _ _) -> (ident, p)) protos


checkConsistency :: KnowledgeBase IRI -> Bool
checkConsistency kb =
  Map.foldlWithKey isMapKeySameAsProtoID True kb
  && Map.foldl (isBaseDefined kb) True kb
  && Map.foldl (\ prev (Proto _ _ adds _ _) -> prev && areChangeValuesDefined adds kb) True kb
  && case isDAG kb of
    Left err -> throw (CycleDetected err)
    Right _ -> True
  -- do not check whether property value referes to P_0, since not possible by type
  -- do not check for double definitions in external KB, since no external KBs implemented


isDAG :: KnowledgeBase IRI -> Either String (Set IRI)
isDAG kb =
  Map.foldl (\ eithergrounded proto ->
    case eithergrounded of
      Left err -> Left err
      Right grounded ->
        case addToBranch kb grounded proto Set.empty of
          Left err -> error err
          Right branch -> Right (Set.union grounded branch)
  ) (Right Set.empty) kb

addToBranch :: KnowledgeBase IRI -> Set IRI -> PrototypeExpression IRI -> Set IRI -> Either String (Set IRI)
addToBranch kb grounded proto@(Proto ident base _ _ _) currentBranch
  | Set.member ident grounded = Right currentBranch
  | Set.member ident currentBranch = Left ("Cycle detected in inheritance tree for: "++ show proto)
  | otherwise =
    let nextBranch = Set.insert ident currentBranch
    in case base of
      Base parent -> addToBranch kb grounded (kb Map.! parent) nextBranch
      P0 -> Right nextBranch

isMapKeySameAsProtoID :: Bool -> IRI -> PrototypeExpression t -> Bool
isMapKeySameAsProtoID prev key (Proto ident _ _ _ _) =
  prev && key == ident ||
  throw (KeysInconsistent ("The key in the KB ('"++ show key ++"') is not equal to the id of the PrototypeDefinition: '"
    ++ show ident++"'"))

isBaseDefined :: KnowledgeBase IRI -> Bool -> PrototypeExpression IRI -> Bool
isBaseDefined kb prev (Proto _ (Base b) _ _ _) =
  prev && ( Map.member b kb || throw (BaseUndefined ("The base '"++show b++"' is not defined")))
isBaseDefined _kb prev _proto = prev


areChangeValuesDefined :: Set (ChangeExpression IRI) -> KnowledgeBase IRI -> Bool
areChangeValuesDefined set kb = Set.foldl (\ prev c -> prev && areValuesDefined c kb) True set

areValuesDefined :: (Ord a1, Show a1) => ChangeExpression a1 -> Map a1 a -> Bool
areValuesDefined (Change pName values) kb =
  Set.foldl (\ prev v -> prev && (Map.member v kb || throw (PropValUndefined ("The value '"++show v++"' of property '"++show pName++"' is not defined.")))) True values
