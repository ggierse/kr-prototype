module Prototype.Specialization where

import Prototype.Basis


changeExpressionIsSpecialization :: SimpleChangeExpression -> SimpleChangeExpression -> Bool
changeExpressionIsSpecialization general special
  | general == special = True
  | otherwise = False
