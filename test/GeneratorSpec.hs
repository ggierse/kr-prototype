module GeneratorSpec (spec) where
import Prototype.Basis
import Prototype.Generator
import Test.Hspec

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


spec :: Spec
spec = do
  describe "generateBaselineChild" $ do
    it "generate child: 1 1" $
      generateBaselineChild 1 1 `shouldBe` (generateComplexId 1 1, Proto {base=Base (ID "http://www.example.com#object0_0"), add=Set.empty, remove=Set.empty, remAll=Set.empty} :: PrototypeExpression IRI)
    it "generate another child: 1 3" $
      generateBaselineChild 1 3 `shouldBe` (generateComplexId 1 3, Proto {base=Base (ID "http://www.example.com#object0_1"), add=Set.empty, remove=Set.empty, remAll=Set.empty} :: PrototypeExpression IRI)
    it "generate one more: 2 6" $
      generateBaselineChild 2 6 `shouldBe` (generateComplexId 2 6, Proto {base=Base (ID "http://www.example.com#object1_3"), add=Set.empty, remove=Set.empty, remAll=Set.empty} :: PrototypeExpression IRI)
  describe "generateBaselineLayer" $ do
    it "generate 1. line" $
      generateBaselineLayer 1 `shouldBe` [
        (ID "http://www.example.com#object1_0",Proto {base = Base (ID "http://www.example.com#object0_0"), add = Set.empty, remove = Set.empty, remAll = Set.empty}),
        (ID "http://www.example.com#object1_1",Proto {base = Base (ID "http://www.example.com#object0_0"), add = Set.empty, remove = Set.empty, remAll = Set.empty})
        ]
    it "generate 2. line" $
      generateBaselineLayer 2 `shouldBe` [
        (ID "http://www.example.com#object2_0",Proto {base = Base (ID "http://www.example.com#object1_0"), add = Set.empty, remove = Set.empty, remAll = Set.empty}),
        (ID "http://www.example.com#object2_1",Proto {base = Base (ID "http://www.example.com#object1_0"), add = Set.empty, remove = Set.empty, remAll = Set.empty}),
        (ID "http://www.example.com#object2_2",Proto {base = Base (ID "http://www.example.com#object1_1"), add = Set.empty, remove = Set.empty, remAll = Set.empty}),
        (ID "http://www.example.com#object2_3",Proto {base = Base (ID "http://www.example.com#object1_1"), add = Set.empty, remove = Set.empty, remAll = Set.empty})
        ]
  describe "generateAllLayers" $ do
    it "generate layers for 1" $
      generateAllLayers [1] `shouldBe` generateBaselineLayer 1
    it "generate layers for 1,2" $
      generateAllLayers [1..2] `shouldBe` generateBaselineLayer 1 ++ generateBaselineLayer 2


  describe "generateBaseline" $ do
      it "generate for n=0" $
        Map.size (generateBaseline 0) `shouldBe` 3
      it "generate for n=1" $
        Map.size (generateBaseline 1) `shouldBe` 3+2^2
      it "generate for n=2" $
        Map.size (generateBaseline 2) `shouldBe` 3+2^2+2^3
      -- TODO: can i do that with quickcheck?
