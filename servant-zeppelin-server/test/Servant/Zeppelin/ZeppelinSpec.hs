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

module Servant.Zeppelin.ZeppelinSpec (spec) where

import Control.Monad.IO.Class
import Control.Monad.Except
import Data.Monoid
import Data.Aeson
import qualified Data.List as L
import GHC.Generics (Generic)
import Servant
import Test.Hspec

import Servant.Zeppelin
import Servant.Zeppelin.Server


spec :: Spec
spec = do
  return ()

--------------------------------------------------------------------------------
-- | Application
--------------------------------------------------------------------------------

type API = "albums" :> Capture "albumId" AlbumId :> Get '[JSON] Album :> SideLoad '[Person, [Photo]]

data AppError = LookupError String

newtype AppHandler a = AppHandler { runAppHandler :: ExceptT AppError IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError AppError)

server :: ServerT API AppHandler
server = albumHandler

albumHandler :: AlbumId -> AppHandler Album
albumHandler aid =
  case L.find (\album -> albumId album == aid) albumTable of
    Nothing -> throwError . LookupError $ "Could not find album with id: " <> show aid
    Just album -> return album

--------------------------------------------------------------------------------
-- | Data
--------------------------------------------------------------------------------
-- | Photo

newtype PhotoId = PhotoId Int
  deriving (Eq, Show, Num, Generic, ToJSON, FromJSON)

type instance NamedDependency [Photo] = "photos"
type instance NamedDependency [PhotoId] = "photoIds"

data Photo =
  Photo { photoId :: PhotoId
        , photoCaption :: String
        , artistId :: PersonId
        } deriving (Eq, Show, Generic)

instance ToJSON Photo
instance FromJSON Photo

photosTable :: [Photo]
photosTable = [ Photo 1 "At the Beach." 1
              , Photo 2 "At the Mountain." 1
              , Photo 3 "With Friends." 1
              , Photo 4 "Bow Wow." 2
              ]

instance Inflatable AppHandler [PhotoId] [Photo] where
  inflator pids = return $ filter (\photo -> photoId photo `elem` pids) photosTable

-- | Person

newtype PersonId = PersonId Int
  deriving (Eq, Show, Num, Generic, ToJSON, FromJSON)

type instance NamedDependency Person = "person"

data Person =
  Person { personId :: PersonId
         , personName :: String
         } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

personTable :: [Person]
personTable = [ Person 1 "Alice"
              , Person 2 "Fido"
              ]

instance Inflatable AppHandler PersonId Person where
  inflator pid =
    case L.find (\person -> personId person == pid) personTable of
      Nothing -> throwError . LookupError $ "Could not find person with id: " <> show pid
      Just person -> return person

-- | Albums

newtype AlbumId = AlbumId Int
  deriving (Eq, Show, Num, ToJSON, FromJSON, FromHttpApiData)

data Album =
  Album { albumId :: AlbumId
        , albumName :: String
        , albumOwner :: PersonId
        , albumPhotos :: [PhotoId]
        } deriving (Eq, Show, Generic)

instance ToJSON Album
instance FromJSON Album

albumTable :: [Album]
albumTable = [ Album 1 "Vacations" 1 [1,2]
             , Album 2 "In the City" 1 [3]
             , Album 3 "Howl" 2 [4]
             ]

instance HasDependencies AppHandler Album '[Person, [Photo]] where
  type DependencyBase Album = '[PersonId, [PhotoId]]
  getDependencies (Album _ _ owner pIds) = (owner &: pIds &: NilDeps)
