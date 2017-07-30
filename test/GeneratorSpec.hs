-----------------------------------------------------------------------------
--
-- Module    :  Prototype.Basis
-- Copyright (C) 2017 Gesche Gierse
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--
-----------------------------------------------------------------------------


module GeneratorSpec (spec) where
import Prototype.Basis
import Prototype.Generator
import qualified Prototype.Serialization as PSerial
import Test.Hspec
import Data.Foldable

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


spec :: Spec
spec = do
  describe "generateBaselineChild" $ do
    it "generate child: 1 1" $
      let ident = generateComplexId 1 1
      in generateBaselineChild 1 1 `shouldBe` (Proto {idIri=ident, base=Base (ID "http://www.example.com#object0_0"), add=Set.empty, remove=Set.empty, remAll=Set.empty} :: PrototypeExpression IRI)
    it "generate another child: 1 3" $
      let ident = generateComplexId 1 3
      in generateBaselineChild 1 3 `shouldBe` (Proto {idIri=ident, base=Base (ID "http://www.example.com#object0_1"), add=Set.empty, remove=Set.empty, remAll=Set.empty} :: PrototypeExpression IRI)
    it "generate one more: 2 6" $
      let ident = generateComplexId 2 6
      in generateBaselineChild 2 6 `shouldBe` (Proto {idIri=ident, base=Base (ID "http://www.example.com#object1_3"), add=Set.empty, remove=Set.empty, remAll=Set.empty} :: PrototypeExpression IRI)
  describe "generateBaselineLayer" $ do
    it "generate 1. line" $
      generateBaselineLayer 1 `shouldBe` [
        Proto {idIri=ID "http://www.example.com#object1_0", base = Base (ID "http://www.example.com#object0_0"), add = Set.empty, remove = Set.empty, remAll = Set.empty},
        Proto {idIri=ID "http://www.example.com#object1_1",base = Base (ID "http://www.example.com#object0_0"), add = Set.empty, remove = Set.empty, remAll = Set.empty}
        ]
    it "generate 2. line" $
      generateBaselineLayer 2 `shouldBe` [
        Proto {idIri=ID "http://www.example.com#object2_0", base = Base (ID "http://www.example.com#object1_0"), add = Set.empty, remove = Set.empty, remAll = Set.empty},
        Proto {idIri=ID "http://www.example.com#object2_1", base = Base (ID "http://www.example.com#object1_0"), add = Set.empty, remove = Set.empty, remAll = Set.empty},
        Proto {idIri=ID "http://www.example.com#object2_2", base = Base (ID "http://www.example.com#object1_1"), add = Set.empty, remove = Set.empty, remAll = Set.empty},
        Proto {idIri=ID "http://www.example.com#object2_3", base = Base (ID "http://www.example.com#object1_1"), add = Set.empty, remove = Set.empty, remAll = Set.empty}
        ]
  describe "generateAllLayers" $ do
    it "generate layers for 1" $
      generateAllLayers [1] `shouldBe` generateBaselineLayer 1
    it "generate layers for 1,2" $
      generateAllLayers [1..2] `shouldBe` generateBaselineLayer 1 ++ generateBaselineLayer 2


  describe "generateBaseline" $ do
    context "size is correct" $ do
      it "generate for n=0" $
        Map.size (generateBaseline 0) `shouldBe` 1
      it "generate for n=1" $
        Map.size (generateBaseline 1) `shouldBe` 3
      it "generate for n=2" $
        Map.size (generateBaseline 2) `shouldBe` 3+2^(2 :: Int)
    context "result is equal to example from java code" $
      forM_ [1,2,3,5] (compareJsonWithHaskell generateBaseline "baseline")


compareJsonWithHaskell :: (Int -> KnowledgeBase IRI) -> String -> Int -> SpecWith ()
compareJsonWithHaskell haskellGeneration kind n = it (kind++", n="++show n) $ PSerial.readKB (getJsonFileName kind n) `shouldReturn` haskellGeneration n


getJsonFileName :: String -> Int -> String
getJsonFileName kind n = "/home/gesche/coding/master-code/example-data/"++kind++"-"++show n++"-new.json"


      -- TODO: can i do that with quickcheck?
