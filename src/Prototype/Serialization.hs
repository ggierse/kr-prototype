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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Prototype.Serialization where

import Prelude hiding (id, rem)
import qualified Prototype.Basis as Base
import Data.Aeson
import Data.Set (Set)
import qualified Data.Map as Map
import GHC.Generics

import qualified Data.ByteString.Lazy as B

data JsonProto = JProto {
  id :: Base.IRI,
  base :: Base.Bases,
  add :: Set (Base.ChangeExpression Base.IRI),
  rem :: Set (Base.ChangeExpression Base.IRI),
  remAll :: Set Base.Property
} deriving (Generic, Show)

instance ToJSON JsonProto where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON JsonProto

instance ToJSON (Base.ChangeExpression Base.IRI)
instance FromJSON (Base.ChangeExpression Base.IRI)

instance ToJSON Base.Property
instance FromJSON Base.Property

instance ToJSON Base.IRI
instance FromJSON Base.IRI

instance ToJSON Base.Bases
instance FromJSON Base.Bases

readAndComputeFixpoint :: [String] -> IO ()
readAndComputeFixpoint [fileName] = do
 -- Get JSON data and decode it
 let kb = readKB fileName
-- Compute the fixpoint
 fixpoints <- Base.computeAllFixpoints <$> kb :: IO (Base.KnowledgeBase Base.IRI)
 -- Show the result
 putStr $ Base.showPretty fixpoints
readAndComputeFixpoint _ = putStr "Error, specify exactly one filename"

readKB :: FilePath -> IO (Base.KnowledgeBase Base.IRI)
readKB path = protosToKB <$> readEntries path

protosToKB :: Maybe [JsonProto] -> Base.KnowledgeBase Base.IRI
protosToKB (Just protos) =
  Map.fromList (map jprotoToKBEntry protos)
protosToKB Nothing = Map.empty

jprotoToKBEntry :: JsonProto -> (Base.IRI, Base.PrototypeExpression Base.IRI)
jprotoToKBEntry JProto {id=name, base=b, add=adds, rem=rems, remAll=remalls} =
  (name, Base.Proto {
    Base.idIri = name,
    Base.base = b,
    Base.add = adds,
    Base.remove = rems,
    Base.remAll = remalls
  })

readEntries :: FilePath -> IO (Maybe [JsonProto])
readEntries path = decode <$> getJSON path

getJSON :: FilePath -> IO B.ByteString
getJSON = B.readFile
