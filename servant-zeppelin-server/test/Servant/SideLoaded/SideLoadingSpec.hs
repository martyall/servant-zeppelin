{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Servant.SideLoaded.SideLoadingSpec (spec) where

import Servant.SideLoaded.Server.Internal.SideLoaded
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), encode, decode, (.:))
import Data.Maybe (isJust)
import Data.Proxy
import GHC.Generics (Generic)
import GHC.TypeLits ()
import Test.Hspec


spec :: Spec
spec = do
  describe "it should inflate albums" $ do
    it "can print the correct result" $ do
      (inflatedAlbum :: _) <- inflate album
      let serializedAlbum = encode . toJSON $ inflatedAlbum
          (mpair :: Maybe (Person, [Photo])) = decode serializedAlbum
      mpair `shouldSatisfy` isJust
      let (Just(o, ps)) = mpair
      o `shouldBe` john
      ps `shouldBe` photos

--------------------------------------------------------------------------------
-- | Photo

newtype PhotoId = PhotoId Int deriving (Eq, Show, Num, ToJSON, FromJSON)

type instance NamedDependency [Photo] = "photos"
type instance NamedDependency [PhotoId] = "photoIds"

data Photo =
  Photo { photoId :: PhotoId
        , photoCaption :: String
        , photoAlbumId :: AlbumId
        , artistId :: PersonId
        } deriving (Eq, Show, Generic)

instance ToJSON Photo
instance FromJSON Photo

photos :: [Photo]
photos = [Photo 1 "At the Beach" 1 1, Photo 2 "In the Mountains" 1 1]

instance Inflatable IO [PhotoId] where
  type Inflated IO [PhotoId] = [Photo]
  inflator = const $ return photos

-- | Person

newtype PersonId = PersonId Int deriving (Eq, Show, Num, ToJSON, FromJSON)

type instance NamedDependency Person = "person"

data Person =
  Person { personId :: PersonId
         , personName :: String
         } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

john :: Person
john = Person 1 "Johnathon"

instance Inflatable IO PersonId where
  type Inflated IO PersonId = Person
  inflator = const $ return john

-- | Albums

newtype AlbumId = AlbumId Int deriving (Eq, Show, Num, ToJSON, FromJSON)

data Album =
  Album { albumId :: AlbumId
        , albumName :: String
        , albumOwner :: PersonId
        , albumPhotos :: [PhotoId]
        } deriving (Eq, Show, Generic)

instance ToJSON Album

instance HasDependencies IO Album '[PersonId, [PhotoId]] where
  getDependencies (Album _ _ owner pIds) = ((owner &: pIds &: NilDeps) :: _)

album :: Album
album = Album 1 "Vacations" 1 [1,2]

--------------------------------------------------------------------------------

instance {-# OVERLAPPING #-} FromJSON (Person, [Photo]) where
  parseJSON (Object o) =
    (,) <$> ((o .: "dependencies") >>= (.: "person"))
        <*> ((o .: "dependencies") >>= (.: "photos"))
  parseJSON _ = fail "could not parse dependencies"

