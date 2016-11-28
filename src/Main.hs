-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Data.Array.Unboxed
import Data.Bits
import Data.Char
import Data.Word

-- This strang looking comment adds code only needed when running the
-- doctest tests embedded in the comments
-- $setup
-- >>> import Data.List (stripPrefix)

-- | Simple function to create a hello message.
-- prop> stripPrefix "Hello " (hello s) == Just s
hello :: String -> String
hello s = "Hello " ++ s

-- data Identifier = ID String
--data Relation = Identifier | Specialization
--data Specialization = Atleast Integer | Atmost Integer
--data Triple = Sentence Identifier Relation Identifier



main :: IO ()
main = putStrLn (hello "World")
