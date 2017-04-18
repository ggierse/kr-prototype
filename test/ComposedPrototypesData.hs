module ComposedPrototypesData where

import Prototype.Basis
import Prototype.Composed
import qualified TestData as Test
import Data.Set (Set)
import qualified Data.Set as Set
--import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntegerInterval



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


generateAllConstraint :: Set IRI -> ConstraintInfo
generateAllConstraint vals =
  TypeConst {constType=AllValuesFrom, constValues=vals}

generateSomeConstraint :: Set IRI -> ConstraintInfo
generateSomeConstraint vals =
  TypeConst {constType=SomeValuesFrom, constValues=vals}


generateCardConstraint :: Integer -> Integer -> ConstraintInfo
generateCardConstraint l u =
  CardinalityConst {constType=Cardinality,
    constInterval=interval (Finite l, True) (Finite u, True)}

generateCardConstraintLower :: Integer -> ConstraintInfo
generateCardConstraintLower l =
  CardinalityConst {constType=Cardinality,
    constInterval=interval (Finite l, True) (PosInf, False)}

generateCardConstraintUpper :: Integer -> ConstraintInfo
generateCardConstraintUpper u =
  CardinalityConst {constType=Cardinality,
    constInterval=interval (Finite 0, True) (Finite u, False)}

prop1 :: IRI
prop1 = ID "_prop1"

prop2 :: IRI
prop2 = ID "_prop2"

prop3 :: IRI
prop3 = ID "_prop3"

prop4 :: IRI
prop4 = ID "_prop4"

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

cardBothDefConstraint :: Prototype IRI
cardBothDefConstraint = generateCardinalityConstraintPrototype CCP {ccId=least2
                          ,cLower=ID "3"
                          ,cUpper=ID "5"}

noLowerConst :: Prototype IRI
noLowerConst = PT { name=least2
    , props=Map.fromList[(upper, Set.singleton (ID "7"))]}

noUpperConst :: Prototype IRI
noUpperConst = PT { name=least2
        , props=Map.fromList[(lower, Set.singleton (ID "7"))]}
lowNotIntConst :: Prototype IRI
lowNotIntConst = generateCardinalityConstraintPrototype CCP {ccId=least2
                          ,cLower=ID "not Int"
                          ,cUpper=ID "5"}

upNotIntConst :: Prototype IRI
upNotIntConst = generateCardinalityConstraintPrototype CCP {ccId=least2
                          ,cLower=ID "2"
                          ,cUpper=ID "not Int"}

upInftyConst :: Prototype IRI
upInftyConst = generateCardinalityConstraintPrototype CCP {ccId=least2
                          ,cLower=ID "2"
                          ,cUpper=ID "proto:infty"}

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

someProperty :: Prototype IRI
someProperty = generatePropertyPrototype PP {pId=prop4
                          ,propName=ID "test:hasSiblings"
                          ,values=Set.empty
                          ,typeConstraints=Set.singleton someBlank
                          ,cardinalityConstraints=Set.empty
                          }

someBlank :: IRI
someBlank = ID "_someBlank"

someConstraint :: Prototype IRI
someConstraint = generateTypeConstraintPrototype TCP {cId=someBlank
                  ,cValues=nameSet
                  ,cType=someValuesFrom
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
                   , (prop4, someProperty)
                   , (someBlank, someConstraint)
                   ]
