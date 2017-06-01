{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}

module Servant.Zeppelin.SwaggerSpec (spec) where

import           Control.Lens               (at, mapped, (&), (?~), (^?), _Just)
import           Data.Aeson
import qualified Data.HashMap.Strict.InsOrd as O
import           Data.Proxy
import qualified Data.Set                   as S
import           Data.Swagger
import           GHC.Generics               (Generic)
import           Servant.API
import           Servant.Swagger
import           Servant.Zeppelin
import           Servant.Zeppelin.Swagger   ()
import           Servant.Zeppelin.Types
import           Test.Hspec                 hiding (example)

spec :: Spec
spec = do
  swaggerSpec
  return ()


swaggerSpec :: Spec
swaggerSpec = describe "Has Swagger instance" $ do

  let swag = toSwagger $ Proxy @API

  it "has properties of the main type" $ do
    let Just (Inline depSchema) =  swag ^? definitions . at "side-loaded JSON: Album"
          . _Just . properties . at "dependencies" . _Just
        Just props = depSchema ^? properties
    (S.fromList . O.keys $ props) `shouldBe` S.fromList ["photos", "person"]


--------------------------------------------------------------------------------
-- | api
--------------------------------------------------------------------------------

type API = "album" :> Get '[JSON] Album :> SideLoad [Person, [Photo]]

--------------------------------------------------------------------------------
-- | Data
--------------------------------------------------------------------------------
-- | Photo

newtype PhotoId = PhotoId Int
  deriving (Eq, Show, Num, Generic, ToJSON, FromJSON)

instance ToSchema PhotoId

type instance NamedDependency [Photo] = "photos"
type instance NamedDependency [PhotoId] = "photoIds"

data Photo =
  Photo { photoId      :: PhotoId
        , photoCaption :: String
        , artistId     :: PersonId
        } deriving (Eq, Show, Generic)

instance ToJSON Photo
instance FromJSON Photo

type instance NamedDependency [Photo] = "photos"

examplePhotos :: Photo
examplePhotos = Photo 1 "At the Beach." 1

instance ToSchema Photo where
  declareNamedSchema p = genericDeclareNamedSchema defaultSchemaOptions p
    & mapped.schema.example ?~ toJSON examplePhotos

-- | Person

newtype PersonId = PersonId Int
  deriving (Eq, Show, Num, Generic, ToJSON, FromJSON)

instance ToSchema PersonId

type instance NamedDependency Person = "person"

data Person =
  Person { personId   :: PersonId
         , personName :: String
         } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

examplePerson :: Person
examplePerson = Person 1 "Alice"

type instance NamedDependency Person = "person"

instance ToSchema Person where
  declareNamedSchema p = genericDeclareNamedSchema defaultSchemaOptions p
    & mapped.schema.example ?~ toJSON examplePerson

-- | Albums

newtype AlbumId = AlbumId Int
  deriving (Eq, Show, Generic, Num, ToJSON, FromJSON)

instance ToSchema AlbumId

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

instance ToSchema Album where
  declareNamedSchema p = genericDeclareNamedSchema defaultSchemaOptions p
    & mapped.schema.example ?~ toJSON exampleAlbum
