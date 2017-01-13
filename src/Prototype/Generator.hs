module Prototype.Generator where

import Prototype.Basis

--mport System.Random

import qualified Data.Bits as Bits
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Control.Monad
import Test.QuickCheck

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

genProtoWithOneProp :: Bases -> Prototype.Basis.Property -> IRI -> PrototypeDefinition IRI
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
      layers = [1..n]
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
generateBlocks :: Int -> Gen [(IRI, PrototypeDefinition IRI)]
generateBlocks = generateAllBlockLayers 100000

generateAllBlockLayers :: Int -> Int -> Gen [(IRI, PrototypeDefinition IRI)]
generateAllBlockLayers numBlocks layers = vectorOfWithLoopNum layers (++) (generateBlockLayer numBlocks)

generateBlockLayer :: Int -> Int -> Gen [(IRI, PrototypeDefinition IRI)]
generateBlockLayer numBlocks layer = vectorOfWithLoopNum numBlocks (:) (generateBlockChild layer numBlocks)

-- calls f with the number of the current loop as first argument
vectorOfWithLoopNum :: (Monad f, Num a, Ord a) => a -> (a1 -> [t] -> [t]) -> (a -> f a1) -> f [t]
vectorOfWithLoopNum cnt0 conc f =
  loop cnt0
  where
    loop cnt
      | cnt < 0 = pure []
      | otherwise = liftM2 conc (f cnt) (loop (cnt - 1))

generateBlockChild :: Int -> Int -> Int -> Gen (IRI, PrototypeDefinition IRI)
generateBlockChild layer numBlocks j  = do
  protodef <- generateBlockPrototypeDefinition layer numBlocks
  return (generateComplexId layer j, protodef)

generateBlockPrototypeDefinition :: Int -> Int -> Gen (PrototypeDefinition IRI)
generateBlockPrototypeDefinition i numBlocks = do
  baseB <- generateBlockBase i numBlocks
  addB <- generateChangeForBlock i numBlocks
  return $ Proto baseB (Set.singleton addB) Set.empty Set.empty

generateChangeForBlock :: Int -> Int -> Gen SimpleChangeExpression
generateChangeForBlock i numBlocks = do
  value <- genIriFromAboveBlock i numBlocks
  return $ Change (Prop $ ID "http://www.example.com#knows") (Set.singleton value)

generateBlockBase :: Int -> Int -> Gen Bases
generateBlockBase 0 _ = return P0
generateBlockBase layer numBlocks = fmap Base (genIriFromAboveBlock layer numBlocks)

genIriFromAboveBlock :: Int -> Int -> Gen IRI
genIriFromAboveBlock 0 _ = return (generateComplexId 0 0)
genIriFromAboveBlock i numBlocks = fmap (generateComplexId (i-1)) (choose (0, numBlocks))

-- use "sample'" to obtain sample, unfortunatly makes it IO

{--
example_generator = create_block_file ("blockgentest", 10, block)


create_block_file (fname, count, gens) =
  do
    test_blocks <- sample' gens
    writeToFile fname (show test_blocks)
    where
      mkDataDef gen_name = liftM BlockLayerBaseDef gen_name

writeToFile name n x = do
  h <- openFile (name ++ "_" ++ n) WriteMode
  hPutStrLn h $ show x
  hClose h
  --}

{--
   * Generates a synthetic prototype KB. The KB is constructed by adding
   * amount prototypes to the KB, one at a time. Each prototype gets a
   * randomly selected earlier created one as its base. Furthermore, each
   * prototype gets between 0 and 4 properties chosen from 10 distinct ones
   * (with replacement). The value of each property is chosen randomly among
   * the prototypes.
--}
generateIncremental :: Int -> Gen [(IRI, PrototypeDefinition IRI)]
generateIncremental n = vectorOfWithLoopNum n (:) generateIncrementalChild

generateIncrementalChild :: Int -> Gen (IRI, PrototypeDefinition IRI)
generateIncrementalChild 0 = return (generateBaseId 0, generateSimplePrototypeDefinition P0)
generateIncrementalChild n = do
  parentId <- choose (0, n-1)
  let cbase = Base $ generateBaseId parentId
  numProps <- choose (0, maxProperties-1)
  cadd <- vectorOf numProps (generateSimpleChange n)
  return (generateBaseId n, createIncrementalProtoDef cbase $ removeDublicateProps cadd)

maxProperties :: Int
maxProperties = 5

generateSimpleChange :: Int -> Gen SimpleChangeExpression
generateSimpleChange n = do
  propertyId <- choose (0, n-1)
  propVal <- choose(0, distinctProperties-1)
  return (Change (Prop $ getPropertyIriN propertyId) (Set.singleton (generateBaseId propVal)))

removeDublicateProps :: [SimpleChangeExpression] -> [SimpleChangeExpression]
removeDublicateProps = foldl (\seen x -> if x `occursWithSameName` seen
                                      then seen
                                      else seen ++ [x]) []

occursWithSameName :: SimpleChangeExpression -> [SimpleChangeExpression] -> Bool
occursWithSameName (Change n _) xs = foldl (\ prev n2 -> prev || n == n2) False $ map (\ (Change pName _) -> pName) xs

distinctProperties :: Int
distinctProperties = 10

getPropertyIriN :: Int -> IRI
getPropertyIriN n = ID $ "http://www.example.com#knows"++show n

createIncrementalProtoDef :: Bases -> [SimpleChangeExpression] -> PrototypeDefinition IRI
createIncrementalProtoDef baseI addI = Proto baseI (Set.fromList addI) Set.empty Set.empty

idListToProtos :: IRI -> [Int] -> [(IRI, PrototypeDefinition IRI)]
idListToProtos propVal = map (genProto propVal 0)

genProto :: IRI -> Int -> Int -> (IRI, PrototypeDefinition IRI)
genProto propVal i j  =
  let prop = Prop (ID "http://www.example.com#knows")
  in
  (generateBaseId i,
  genProtoWithOneProp  (iriToBase $ generateComplexId i j) prop propVal)
