module SpecializationSpec (spec) where
import Prototype.Basis
import Prototype.Specialization
import Test.Hspec

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "isSpecialization" $ do
        it "placeholder" $
            True `shouldBe` True
