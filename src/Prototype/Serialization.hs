{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Prototype.Serialization where

import Prelude hiding (id, rem)
import qualified Prototype.Basis as Base
import Data.Aeson
import qualified Data.Set as Set
import qualified Data.Map as Map
import GHC.Generics

import qualified Data.ByteString.Lazy as B

data JsonProto = JProto {
  id :: Base.IRI,
  base :: Base.Bases,
  add :: Set.Set (Base.ChangeExpression Base.IRI),
  rem :: Set.Set (Base.ChangeExpression Base.IRI),
  remAll :: Set.Set Base.Property
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

jsonFile :: FilePath
jsonFile = "test.json"

readKB :: FilePath -> IO (Base.KnowledgeBase Base.IRI)
readKB path = protosToKB <$> readEntries path

protosToKB :: Maybe [JsonProto] -> Base.KnowledgeBase Base.IRI
protosToKB (Just protos) =
  Map.fromList (map jprotoToKBEntry protos)
protosToKB Nothing = Map.empty

jprotoToKBEntry :: JsonProto -> (Base.IRI, Base.PrototypeExpression Base.IRI)
jprotoToKBEntry JProto {id=name, base=b, add=adds, rem=rems, remAll=remalls} =
  (name, Base.Proto {
    Base.base = b,
    Base.add = adds,
    Base.remove = rems,
    Base.remAll = remalls
  })

readEntries :: FilePath -> IO (Maybe [JsonProto])
readEntries path = decode <$> getJSON

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile
