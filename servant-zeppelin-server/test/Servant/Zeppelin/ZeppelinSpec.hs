{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Servant.Zeppelin.ZeppelinSpec (spec) where

import           Control.Lens             ((&), (.~), (^.), (^?))
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.List                as L
import           Data.Monoid
import           Data.String.Conversions
import           GHC.Generics             (Generic)
import qualified Network.HTTP.Client      as HCli
import           Network.HTTP.Types       (Status, status200, status404)
import           Network.Wai.Handler.Warp (testWithApplication)
import           Network.Wreq             (defaults, getWith, header, param,
                                           responseBody, responseStatus)
import           Servant                  hiding (header)
import           Servant.Utils.Enter
import           Test.Hspec
import           Test.QuickCheck

import           Servant.Zeppelin
import           Servant.Zeppelin.Server


spec :: Spec
spec = do
  zeppelinSpec
  contentTypeSpec
  return ()

zeppelinSpec :: Spec
zeppelinSpec
  = describe "Can sideload album data based on query flag"
  $ around (testWithApplication . return $ app) $ do

    it "can side load inflated data in the presence of the query flag" $ \port -> quickCheckWith qcArgs $ property $
                                                                         \(aid :: AlbumId) -> do
      let album = getAlbumById aid
          (AlbumId aidInt) = aid
          path = "/albums/" <> show aidInt
          options = defaults & param "sideload" .~ ["true"]
      resp <- getWith options $ url path port
      resp ^? responseBody . key "data" . _JSON `shouldBe` album
      resp ^? responseBody . key "dependencies" . key "person" . _JSON
        `shouldBe` (getPersonById . albumOwner =<< album)
      resp ^? responseBody . key "dependencies" . key "photos" . _JSON
        `shouldBe` (getPhotosByIds . albumPhotos <$> album)

    it "can side load normal data in the absense of the query flag" $ \port -> quickCheckWith qcArgs $ property $
                                                                      \(aid :: AlbumId) -> do
      let album = getAlbumById aid
          (AlbumId aidInt) = aid
          path = "/albums/" <> show aidInt
      resp <- getWith defaults $ url path port
      resp ^? responseBody . _JSON `shouldBe` album

    it "will throw an appropriate error for a missing dependency when sideloading" $ \port -> do
      let path = "/albums/4"
          options = defaults & param "sideload" .~ ["true"]
      getWith options (url path port) `shouldHTTPErrorWith` status404

    it "doesn't care about missing dependency if not side loading" $ \port -> do
      let path = "/albums/4"
      resp <- getWith defaults (url path port)
      resp ^. responseStatus `shouldBe` status200

contentTypeSpec :: Spec
contentTypeSpec
  = describe "Can handle content types appropriately"
  $ around (testWithApplication . return $ app) $ do

-- the use of defaults above already tests the wildcard case.
    it "can handle application/json" $ \port -> do

      let album = getAlbumById 1
          path = "/albums/1"
          options = defaults
            & header "Accept" .~ ["application/json"]
            & param "sideload" .~ ["true"]
      resp <- getWith options $ url path port
      resp ^? responseBody . key "data" . _JSON `shouldBe` album
      resp ^? responseBody . key "dependencies" . key "person" . _JSON
        `shouldBe` (getPersonById . albumOwner =<< album)
      resp ^? responseBody . key "dependencies" . key "photos" . _JSON
        `shouldBe` (getPhotosByIds . albumPhotos <$> album)

    it "can handle other content types" $ \port -> do
      let options = defaults
            & header "Accept" .~ ["text/plain;charset=utf-8"]
            & param "sideload" .~ ["true"]
          path = "/albums/1"
      respWithParam <- getWith options $ url path port
      respWithParam ^? responseBody `shouldBe` Just "Album"
      respWithoutParam <- getWith options $ url path port
      respWithoutParam ^? responseBody `shouldBe` Just "Album"

qcArgs :: Args
qcArgs = stdArgs {maxSuccess = 20}

--------------------------------------------------------------------------------
-- | Application
--------------------------------------------------------------------------------

type API = "albums" :> Capture "albumId" AlbumId
                    :> Get '[JSON, PlainText] Album
                    :> SideLoad '[Person, [Photo]]

newtype QueryError = LookupError String

newtype DB a = DB { runDB :: ExceptT QueryError IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError QueryError)

server :: ServerT API Handler
server = albumHandler

albumHandler :: AlbumId -> Handler Album
albumHandler aid =
  case L.find (\album -> albumId album == aid) albumTable of
    Nothing    -> throwError err404
    Just album -> return album

phi :: DB :~> Handler
phi = NT $ \ha -> do
  ea <- liftIO . runExceptT . runDB $ ha
  case ea of
    Left (LookupError msg) -> throwError err404 {errBody = cs msg}
    Right a                -> return a

app :: Application
app = serveWithContext (Proxy @API) ctxt server
  where
    ctxt :: Context '[DB :~> Handler]
    ctxt = phi :. EmptyContext


url :: String -> Int -> String
url path port = "http://localhost:" <> show port <> path

shouldHTTPErrorWith :: IO a -> Status -> Expectation
shouldHTTPErrorWith act stat = act `shouldThrow` \e -> case e of
  HCli.HttpExceptionRequest _ (HCli.StatusCodeException resp _)
    -> HCli.responseStatus resp == stat
  _ -> False

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
  Person { personId   :: PersonId
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
  Album { albumId     :: AlbumId
        , albumName   :: String
        , albumOwner  :: PersonId
        , albumPhotos :: [PhotoId]
        } deriving (Eq, Show, Generic)

instance ToJSON Album
instance FromJSON Album

albumTable :: [Album]
albumTable = [ Album 1 "Vacations" 1 [1,2]
             , Album 2 "In the City" 1 [3]
             , Album 3 "Howl" 2 [4]
             , Album 4 "Lost Memories" 3 [1,3] -- person foreign key error
             ]

getAlbumById :: AlbumId -> Maybe Album
getAlbumById aid = L.find (\album -> albumId album == aid) albumTable

instance HasDependencies DB Album '[PersonId, [PhotoId]] where
  getDependencies (Album _ _ owner pIds) = owner :&: pIds :&: NilDeps

instance MimeRender PlainText Album where
  mimeRender _ _ = "Album"

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
