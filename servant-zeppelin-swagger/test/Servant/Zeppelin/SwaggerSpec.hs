{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Servant.Zeppelin.SwaggerSpec (spec) where

import Control.Lens
import  Data.Aeson
import  GHC.Generics
import           Test.Hspec
import           Servant.Zeppelin.Types

spec :: Spec
spec = do
  return ()

--------------------------------------------------------------------------------
-- | Data
--------------------------------------------------------------------------------
-- | Photo

newtype PhotoId = PhotoId Int
  deriving (Eq, Show, Num, Generic, ToJSON, FromJSON)

type instance NamedDependency [Photo] = "photos"
type instance NamedDependency [PhotoId] = "photoIds"

data Photo =
  Photo { photoId      :: PhotoId
        , photoCaption :: String
        , artistId     :: PersonId
        } deriving (Eq, Show, Generic)

instance ToJSON Photo
instance FromJSON Photo

examplePhotos :: [Photo]
examplePhotos = [ Photo 1 "At the Beach." 1
                , Photo 2 "At the Mountain." 1
                ]

-- | Person

newtype PersonId = PersonId Int
  deriving (Eq, Show, Num, Generic, ToJSON, FromJSON)

type instance NamedDependency Person = "person"

data Person =
  Person { personId   :: PersonId
         , personName :: String
         } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

examplePerson :: Person
examplePerson = Person 1 "Alice"

-- | Albums

newtype AlbumId = AlbumId Int
  deriving (Eq, Show, Num, ToJSON, FromJSON)

data Album =
  Album { albumId     :: AlbumId
        , albumName   :: String
        , albumOwner  :: PersonId
        , albumPhotos :: [PhotoId]
        } deriving (Eq, Show, Generic)

instance ToJSON Album
instance FromJSON Album

exampleAlbum :: Album
exampleAlbum = Album 1 "Vacations" 1 [1,2]

exampleSideLoaded :: SideLoaded Album '[Person, [Photo]]
exampleSideLoaded =
  let deps = examplePerson :&: (examplePhotos :&: NilDeps)
  in SideLoaded exampleAlbum deps
