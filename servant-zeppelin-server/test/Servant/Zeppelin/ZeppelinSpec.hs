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

import Control.Lens ((^?))
import Control.Monad.IO.Class
import Control.Monad.Except
import Data.Monoid
import Data.Aeson
import           Data.Aeson.Lens

import Data.String.Conversions
import           Network.Wai.Handler.Warp            (testWithApplication)
import           Network.Wreq                        (defaults, getWith, responseBody)

import qualified Data.List as L
import GHC.Generics (Generic)
import Servant
-- import Servant.Utils.Enter
import Test.Hspec
import Test.QuickCheck

import Servant.Zeppelin
import Servant.Zeppelin.Server


spec :: Spec
spec = do
  zeppelinSpec
  return ()

zeppelinSpec :: Spec
zeppelinSpec
  = describe "Can sideload album data based on query flag"
  $ around (testWithApplication . return $ app) $ do

    it "can side load data in the presence of the query flag" $ \port -> property $
                                                                \(aid :: AlbumId) -> do
      let album = getAlbumById aid
          (AlbumId aidInt) = aid
          path = "/albums/" <> show aidInt
      resp <- getWith defaults $ url (path <> "?sideload") port
      resp ^? responseBody . key "data" . _JSON `shouldBe` getAlbumById aid
      resp ^? responseBody . key "dependencies" . key "person" . _JSON
        `shouldBe` (getPersonById . albumOwner =<< album)
      resp ^? responseBody . key "dependencies" . key "photos" . _JSON
        `shouldBe` (getPhotosByIds . albumPhotos <$> album)


--------------------------------------------------------------------------------
-- | Application
--------------------------------------------------------------------------------

type API = "albums" :> Capture "albumId" AlbumId :> Get '[JSON] Album :> SideLoad

data QueryError = LookupError String

newtype DB a = DB { runDB :: ExceptT QueryError IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError QueryError)

server :: ServerT API Handler
server = albumHandler

albumHandler :: AlbumId -> Handler Album
albumHandler aid =
  case L.find (\album -> albumId album == aid) albumTable of
    Nothing -> throwError err404
    Just album -> return album

phi :: DB :~> Handler
phi = NT $ \ha -> do
  ea <- liftIO . runExceptT . runDB $ ha
  case ea of
    Left (LookupError msg) -> throwError err404 {errBody = cs msg}
    Right a -> return a

app :: Application
app = serveWithContext (Proxy @API) ctxt server
  where
    ctxt :: Context '[DB :~> Handler]
    ctxt = phi :. EmptyContext


url :: String -> Int -> String
url path port = "http://localhost:" <> show port <> path

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

getPhotosByIds :: [PhotoId] -> [Photo]
getPhotosByIds pids = filter (\photo -> photoId photo `elem` pids) photosTable

instance Inflatable DB [PhotoId] where
  type Full DB [PhotoId] = [Photo]
  inflator = return . getPhotosByIds

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

getPersonById :: PersonId -> Maybe Person
getPersonById pid = L.find (\person -> personId person == pid) personTable

instance Inflatable DB PersonId where
  type Full DB PersonId = Person
  inflator pid =
    case getPersonById pid of
      Nothing -> throwError . LookupError $ "Could not find person with id: " <> show pid
      Just person -> return person

-- | Albums

newtype AlbumId = AlbumId Int
  deriving (Eq, Show, Num, ToJSON, FromJSON, FromHttpApiData)

instance Arbitrary AlbumId where
  arbitrary = elements $ map AlbumId [1..3]

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

getAlbumById :: AlbumId -> Maybe Album
getAlbumById aid = L.find (\album -> albumId album == aid) albumTable

instance HasDependencies DB Album '[PersonId, [PhotoId]] where
  getDependencies (Album _ _ owner pIds) = (owner &: pIds &: NilDeps)
