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

--instance ToJSON [JsonProto]
--instance FromJSON [JsonProto]

instance ToJSON (Base.ChangeExpression Base.IRI)
instance FromJSON (Base.ChangeExpression Base.IRI)

instance ToJSON Base.Property
instance FromJSON Base.Property

instance ToJSON Base.IRI
instance FromJSON Base.IRI

instance ToJSON Base.Bases
instance FromJSON Base.Bases

carInterm = JProto {
  Prototype.Serialization.id = Base.ID "test",
  base = Base.P0,
  add = Set.fromList [Base.Change (Base.Prop (Base.ID "hasName")) (Set.singleton (Base.ID "myname"))],
  Prototype.Serialization.rem = Set.empty,
  remAll = Set.empty
}
{--
readKnowledgeBase :: FilePath -> Base.KnowledgeBase Base.IRI
readKnowledgeBase path =
  let jsonByteString = B.readFile path
      protos = parseListOfProtos jsonByteString
  in protosToKB protos

parseListOfProtos :: IO B.ByteString -> Maybe [JsonProto]
parseListOfProtos string = do
  s <- string
  case s of
    Left err -> Nothing
    Right res -> Just (decode res)
--}
protosToKB :: [JsonProto] -> Base.KnowledgeBase Base.IRI
protosToKB protos =
  Map.fromList (map jprotoToKBEntry protos)

jprotoToKBEntry :: JsonProto -> (Base.IRI, Base.PrototypeExpression Base.IRI)
jprotoToKBEntry JProto {id=name, base=b, add=adds, rem=rems, remAll=remalls} =
  (name, Base.Proto {
    Base.base = b,
    Base.add = adds,
    Base.remove = rems,
    Base.remAll = remalls
  })

jsonFile :: FilePath
jsonFile = "test.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO ()
main = do
 -- Get JSON data and decode it
 d <- (decode <$> getJSON) :: IO (Maybe [JsonProto])
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
  Nothing -> putStrLn "error parsing"
  Just ps -> print (protosToKB ps)
