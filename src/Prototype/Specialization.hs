module Prototype.Specialization where

import Prototype.Basis

import qualified Data.Set as Set


changeExpressionIsSpecializationOf :: SimpleChangeExpression -> SimpleChangeExpression -> Bool
changeExpressionIsSpecializationOf special@(Change propS iriSetS) general@(Change propG iriSetG)
  | propS /= propG = False
  | general == special = True
  | iriSetS `Set.isSubsetOf` iriSetG = True
  | otherwise = False
