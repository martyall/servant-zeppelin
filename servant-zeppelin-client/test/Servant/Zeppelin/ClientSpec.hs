{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds#-}

module Servant.Zeppelin.ClientSpec (spec) where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Default.Class
import           Data.Either
import qualified Data.List                            as L
import           Data.String.Conversions
import           GHC.Generics
import           Network.Wai.Middleware.RequestLogger
import           Servant.Server
import           Servant.Zeppelin
import           Servant.Zeppelin.Client
import           Servant.Zeppelin.Server
import           Servant.Zeppelin.Types

import           Network.Wai.Handler.Warp             (testWithApplication)
import           System.IO.Unsafe                     (unsafePerformIO)


import           Network.HTTP.Client                  (Manager,
                                                       defaultManagerSettings,
                                                       newManager)
import           Servant
import           Servant.Client
import           Test.Hspec
import           Test.QuickCheck


spec :: Spec
spec = do
  hasClientSpec
  return ()

hasClientSpec :: Spec
hasClientSpec = describe "HasClient" $ around (testWithApplication app) $ do

  it "succeeds when we ask for the inflated data" $ \port -> property
                                                  $ \aid -> do
    ealbum <- getAlbumClientFull mgr (BaseUrl Http "localhost" port "") aid
    let Just (person, photos) = do
          a <- getAlbumById aid
          p <- getPersonById (albumOwner a)
          let phs = getPhotosByIds (albumPhotos a)
          return (p, phs)
    ealbum `shouldSatisfy` isRight
    let Right (SideLoaded a deps) = ealbum
    person `shouldBe` projectDependency deps
    photos `shouldBe` projectDependency deps

  it "succeeds when we ask for the uninflated data" $ \port -> property
                                                    $ \aid -> do
    ealbum <- getAlbumClient mgr (BaseUrl Http "localhost" port "") aid
    ealbum `shouldSatisfy` isRight
    let Right album = ealbum
        Just album' = getAlbumById aid
    album `shouldBe` album'

--------------------------------------------------------------------------------
-- | Client
--------------------------------------------------------------------------------

mgr :: Manager
mgr = unsafePerformIO $ newManager defaultManagerSettings
{-# NOINLINE mgr #-}

type AlbumDeps =  '[Person, [Photo]]

getAlbumClientFull :: Manager
                   -> BaseUrl
                   -> AlbumId
                   -> IO (Either ServantError (SideLoaded Album AlbumDeps))
getAlbumClientFull m burl aid =
  flip runClientM (ClientEnv m burl) $
    runDepClient (client api aid) STrue

getAlbumClient :: Manager
               -> BaseUrl
               -> AlbumId
               -> IO (Either ServantError Album)
getAlbumClient m burl aid =
  flip runClientM (ClientEnv m burl) $
    runDepClient (client api aid) SFalse

--------------------------------------------------------------------------------
-- | Application
--------------------------------------------------------------------------------

type API = "albums" :> Capture "albumId" AlbumId
                    :> Get '[JSON, PlainText] Album
                    :> SideLoad '[Person, [Photo]]

api :: Proxy API
api = Proxy @API

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

loggerOptions :: RequestLoggerSettings
loggerOptions = def {outputFormat = Detailed True}

app :: IO Application
app = do
  logger <- mkRequestLogger loggerOptions
  return . logger $ serveWithContext api ctxt server
  where
    ctxt :: Context '[DB :~> Handler]
    ctxt = phi :. EmptyContext

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
  deriving (Eq, Show, Num, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)

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
             ]

getAlbumById :: AlbumId -> Maybe Album
getAlbumById aid = L.find (\album -> albumId album == aid) albumTable

instance HasDependencies DB Album '[PersonId, [PhotoId]] where
  getDependencies (Album _ _ owner pIds) = owner &: pIds &: NilDeps

instance MimeRender PlainText Album where
  mimeRender _ _ = "Album"
