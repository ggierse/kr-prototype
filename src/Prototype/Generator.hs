module Prototype.Generator where

import Prototype.Basis

--mport System.Random

import qualified Data.Bits as Bits
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as List

generateBaseId :: Int -> IRI
generateBaseId n = ID ("http://www.example.com#object" ++ show n)

generateComplexId :: Int -> Int -> IRI
generateComplexId parent child = ID ("http://www.example.com#object"++ show parent++"_"++ show child)

generateSimplePrototypeDefinition :: Bases -> PrototypeDefinition IRI
generateSimplePrototypeDefinition b =
  Proto {
      base = b,
      add = Set.empty,
      remove = Set.empty,
      remAll = Set.empty
    }

genProtoWithOneProp :: Bases -> Property -> IRI -> PrototypeDefinition IRI
genProtoWithOneProp s p o =
  Proto {
      base = s,
      add = Set.singleton (Change p (Set.singleton o)),
      remove = Set.empty,
      remAll = Set.empty
    }

{--
  * Generates a synthetic prototype KB. To generate the data we start with
  * one prototype which derives from {@link Prototype#P_0}, next we create
  * two prototypes which derive form the one, then we create four prototypes
  * which derive from these two, and so on until we create 2^n prototypes
  * which derive from 2^(n-1)
--}
generateBaseline :: Int -> KnowledgeBase IRI
generateBaseline n =
  let first =  (generateComplexId 0 0, generateSimplePrototypeDefinition P0)
      layers = [1..(n+1)]
  --in Map.insert ident first $ Map.unions $ generateAllLayers layers
  --in List.foldr (\ prev (key,value) -> Map.insert key value prev ) first (generateAllLayers layers)
  -- [Map IRI KnowledgeBase] -> Map IRI KnowledgeBase
  in Map.fromList (first : generateAllLayers layers)

generateAllLayers :: [Int] -> [(IRI, PrototypeDefinition IRI)]
generateAllLayers = concatMap generateBaselineLayer

generateBaselineLayer :: Int -> [(IRI, PrototypeDefinition IRI)]
generateBaselineLayer i =
  let numChildren = [0..(2^i-1)]
  in map (generateBaselineChild i) numChildren

generateBaselineChild :: Int -> Int -> (IRI, PrototypeDefinition IRI)
generateBaselineChild i j =
  let basis = Base (generateComplexId (i-1) (Bits.shift j (-1)))
  in (generateComplexId i j, generateSimplePrototypeDefinition basis)

{-- Generates a synthetic prototype KB. Creates n blocks of 100,000
  * prototypes. All prototypes in each block derive from a randomly chosen
  * prototype in a lower block. Then, each of the prototypes has a property
  * with a value randomly chosen from the block below. In the lowest block,
  * the base is always P_0 and the value for the property is always the same
  * fixed prototype.
--}
generateBlocks :: Int -> KnowledgeBase IRI
generateBlocks n =
  let numberPerLayer = 100000
      firstValue = generateComplexId 0 0
      property = Prop (ID "http://www.example.com#knows")
      baseLayer = idListToProtos firstValue [1..numberPerLayer]
  in Map.empty

generateBlockLayers :: [Int] -> [(IRI, PrototypeDefinition IRI)]
generateBlockLayers layers = []

{--
generateBlockLayerItem :: Int -> Int -> Int -> (IRI, PrototypeDefinition IRI)
generateBlockLayerItem i j numberPerLayer =
  let parentnum = Quick.choose (0,numberPerLayer)
      base = generateComplexId (i-1)
  in (ID "", generateSimplePrototypeDefinition (Base (ID "")))

--}


idListToProtos :: IRI -> [Int] -> [(IRI, PrototypeDefinition IRI)]
idListToProtos propVal = map (genProto propVal 0)

genProto :: IRI -> Int -> Int -> (IRI, PrototypeDefinition IRI)
genProto propVal i j  =
  let property = Prop (ID "http://www.example.com#knows")
  in
  (generateBaseId i,
  genProtoWithOneProp  (iriToBase $ generateComplexId i j) property propVal)
