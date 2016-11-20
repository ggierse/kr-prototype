-----------------------------------------------------------------------------
--
-- Module      :  Prototypemain
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

module Prototypemain (
    main,
    encode
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

-- syntax

-- semantik
data IRI = ID String deriving (Show)
data Property = Prop IRI deriving (Show)
data Bases = Base IRI | P0 deriving (Show)
data SimpleChangeExpression = Change Property [IRI] | Empty deriving (Show)
data PrototypeExpression = Proto {identity :: IRI, base :: Bases, add :: SimpleChangeExpression, remove :: SimpleChangeExpression}


-- Convert IRI to Bases type
--
-- |
-- >>> iriToBase (ID "test:test")
-- Base (ID "test:test")
iriToBase :: IRI -> Bases
iriToBase iri = Base iri

isFixPoint :: PrototypeExpression -> Bool
isFixPoint (Proto {identity=_, base=_, add=_, remove = Empty}) = True
isFixPoint _ = False

main :: IO ()
main = putStrLn (hello "World")



-- |
-- Base64 encoding.
--
-- >>> encode "foo bar"
-- "Zm9vIGJhcg=="
--
encode :: String -> String
encode = map (base64array !) . encode' . map (fromIntegral . ord)

encode' :: [Word8] -> [Word8]
encode' []         = []
encode' [a]        = e1 a : e2 a 0 : pad    : pad  : []
encode' [a,b]      = e1 a : e2 a b : e3 b 0 : pad  : []
encode' (a:b:c:xs) = e1 a : e2 a b : e3 b c : e4 c : encode' xs

e1,e4 :: Word8 -> Word8
e2,e3 :: Word8 -> Word8 -> Word8
e1 a   = shiftR a 2
e2 a b = shiftL (a .&. 0x03) 4 .|. shiftR b 4
e3 b c = shiftL (b .&. 0x0f) 2 .|. shiftR c 6
e4   c = c .&. 0x3f

base64 :: String
base64 = ['A'..'Z']++['a'..'z']++['0'..'9']++"+/="

base64array :: UArray Word8 Char
base64array = array (0,pad) $ zip [0..pad] base64

pad :: Word8
pad = 64



