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

data TypeConstraintPrototype = TCP {
   cId :: IRI
  ,cValues :: Set IRI
  ,cType :: IRI
}

data CardinalityConstraintPrototype = CCP {
  ccId :: IRI
  ,cLower :: IRI
  ,cUpper :: IRI
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

generateTypeConstraintPrototype :: TypeConstraintPrototype -> Prototype IRI
generateTypeConstraintPrototype tcp =
  PT { name=cId tcp
     , props=Map.fromList[(hasConstraintType, Set.singleton $ cType tcp)
                         ,(hasConstraintValue, cValues tcp)]}

generateCardinalityConstraintPrototype :: CardinalityConstraintPrototype -> Prototype IRI
generateCardinalityConstraintPrototype ccp =
 PT { name=ccId ccp
    , props=Map.fromList[(lower, Set.singleton $ cLower ccp)
                        ,(upper, Set.singleton $ cUpper ccp)]}

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

childLeast2Property :: Prototype IRI
childLeast2Property = generatePropertyPrototype PP {pId=prop1
                           ,propName=ID "test:hasChildren"
                           ,values=Set.empty
                           ,typeConstraints=Set.empty
                           ,cardinalityConstraints=Set.singleton least2
                         }
childLeast2Constraint :: Prototype IRI
childLeast2Constraint = generateCardinalityConstraintPrototype CCP {ccId=least2
                          ,cLower=ID "2"
                          ,cUpper=infty}

namesAllFromProperty :: Prototype IRI
namesAllFromProperty = generatePropertyPrototype PP {pId=prop2
                          ,propName=ID "test:hasName"
                          ,values=Set.empty
                          ,typeConstraints=Set.singleton fromNameSet
                          ,cardinalityConstraints=Set.empty
                        }
nameSet :: Set IRI
nameSet = Set.fromList [Test.jan, Test.tad, Test.susan, Test.frank, Test.tamara]
namesAllFromConstraint :: Prototype IRI
namesAllFromConstraint = generateTypeConstraintPrototype TCP {cId=fromNameSet
                            ,cValues=nameSet
                            ,cType=allValuesFrom}

mixedValues :: Set IRI
mixedValues = Set.singleton $ ID "test:hans"

mixedProperty :: Prototype IRI
mixedProperty = generatePropertyPrototype PP {pId=prop3
                          ,propName=ID "test:hasSiblings"
                          ,values=mixedValues
                          ,typeConstraints=Set.singleton fromNameSet
                          ,cardinalityConstraints=Set.empty
                        }



genProto :: Prototype IRI
genProto = PT {name=Test.parent, props=generateHasProperty $ Set.fromList [prop1, prop2, prop3]}

fkb :: FixpointKnowledgeBase IRI
fkb = Map.fromList [ (Test.parent, genProto)
                   , (prop1, childLeast2Property)
                   , (prop2, namesAllFromProperty)
                   , (prop3, mixedProperty)
                   , (fromNameSet, namesAllFromConstraint)
                   , (least2, childLeast2Constraint)
                   ]
