module ComposedPrototypesData where

import Prototype.Basis
import Prototype.Specialization
import qualified TestData as Test
import Data.Set (Set)
import qualified Data.Set as Set
--import Data.Map (Map)
import qualified Data.Map as Map



generateHasProperty :: Set IRI -> PropertyMap IRI
generateHasProperty  = Map.singleton hasProperty



data PropertyPrototype = PP {
   pId :: IRI
  ,propName :: IRI
  ,values :: Set IRI
  ,typeConstraints :: Set IRI
  ,cardinalityConstraints :: Set IRI
}

generatePropertyPrototype :: PropertyPrototype -> Prototype IRI
generatePropertyPrototype pp =
  PT { name=pId pp
     , props=Map.fromList[(hasID, Set.singleton $ propName pp)
                              ,(hasValue, values pp)
                              ,(hasTypeConstraint, typeConstraints pp)
                              ,(hasCardinalityConstraint, cardinalityConstraints pp)
                              ]
}

prop1 :: IRI
prop1 = ID "_prop1"

prop2 :: IRI
prop2 = ID "_prop2"

prop3 :: IRI
prop3 = ID "_prop3"

least2 :: IRI
least2 = ID "_least2card"
fromNameSet :: IRI
fromNameSet = ID "_fromNameSet"
mixed :: IRI
mixed = ID "_mixed"

childLeast2Constraint :: Prototype IRI
childLeast2Constraint = generatePropertyPrototype PP {pId=prop1
                           ,propName=ID "test:hasChildren"
                           ,values=Set.empty
                           ,typeConstraints=Set.empty
                           ,cardinalityConstraints=Set.singleton least2
                         }
namesAllFromConstraint :: Prototype IRI
namesAllFromConstraint = generatePropertyPrototype PP {pId=prop2
                          ,propName=ID "test:hasName"
                          ,values=Set.empty
                          ,typeConstraints=Set.singleton fromNameSet
                          ,cardinalityConstraints=Set.empty
                        }

mixedValues :: Set IRI
mixedValues = Set.singleton $ ID "test:hans"

mixedConstraint :: Prototype IRI
mixedConstraint = generatePropertyPrototype PP {pId=prop3
                          ,propName=ID "test:hasSiblings"
                          ,values=mixedValues
                          ,typeConstraints=Set.singleton fromNameSet
                          ,cardinalityConstraints=Set.empty
                        }


genProto :: Prototype IRI
genProto = PT {name=Test.parent, props=generateHasProperty $ Set.fromList [prop1, prop2]}

fkb :: FixpointKnowledgeBase IRI
fkb = Map.fromList [ (Test.parent, genProto)
                   , (prop1, childLeast2Constraint)
                   , (prop2, namesAllFromConstraint)
                   ]
