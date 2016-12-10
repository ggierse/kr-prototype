module Prototype.Generator where

import Prototype.Basis

--import Test.QuickCheck

import qualified Data.Bits as Bits
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List as List

generateBaseId :: Int -> IRI
generateBaseId n = ID ("http://www.example.com#object" ++ show n)

generateComplexId :: Int -> Int -> IRI
generateComplexId parent child = ID ("http://www.example.com#object"++ show parent++"_"++ show child)

generateSimplePrototypeExpression :: Bases -> PrototypeExpression IRI
generateSimplePrototypeExpression b =
  Proto {
      base = b,
      add = Set.empty,
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
  let first =  (generateComplexId 0 0, generateSimplePrototypeExpression P0)
      layers = [1..(n+1)]
  --in Map.insert ident first $ Map.unions $ generateAllLayers layers
  --in List.foldr (\ prev (key,value) -> Map.insert key value prev ) first (generateAllLayers layers)
  -- [Map IRI KnowledgeBase] -> Map IRI KnowledgeBase
  in Map.fromList (first : generateAllLayers layers)



generateAllLayers :: [Int] -> [(IRI, PrototypeExpression IRI)]
generateAllLayers = concatMap generateBaselineLayer

generateBaselineLayer :: Int -> [(IRI, PrototypeExpression IRI)]
generateBaselineLayer i =
  let numChildren = [0..(2^i-1)]
  in map (generateBaselineChild i) numChildren

generateBaselineChild :: Int -> Int -> (IRI, PrototypeExpression IRI)
generateBaselineChild i j =
  let basis = Base (generateComplexId (i-1) (Bits.shift j (-1)))
  in (generateComplexId i j, generateSimplePrototypeExpression basis)

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
      baseLayer = idListToProtos [1..numberPerLayer]
  in Map.empty

idListToProtos :: [Int] -> [(IRI, PrototypeExpression IRI)]
idListToProtos = map (genProto 0)

genProto :: Int -> Int -> (IRI, PrototypeExpression IRI)
genProto i j =
  (generateBaseId i,
  generateSimplePrototypeExpression  (iriToBase $ generateComplexId i j))
