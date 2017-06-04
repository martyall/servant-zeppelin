{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Zeppelin.Client
  ( ProjectDependency(..)
  , DepClient(..)
  -- Re-Exported from Data.Singletons
  , SBool
  , Apply
  ) where

import           Data.Aeson
import           Data.Functor.Identity
import           Data.Kind
import           Data.Proxy
import           Data.Singletons.Prelude hiding ((:>))
import           Data.Singletons.TypeLits
import qualified Data.Text                as T
import           Servant.API
import           Servant.Client
import           Servant.Common.Req

import           Servant.Zeppelin
import           Servant.Zeppelin.Types

--------------------------------------------------------------------------------
-- | FromJSON Instances
--------------------------------------------------------------------------------

instance FromJSON (DependencyList Identity '[] '[]) where
  parseJSON (Object _) = return NilDeps
  -- this can't actually happen.
  parseJSON _ = fail "Nil dependency list should be the empty object."

instance ( FromJSON (DependencyList Identity ds ds)
         , KnownSymbol (NamedDependency d)
         , FromJSON d
         ) => FromJSON (DependencyList Identity (d : ds) (d : ds)) where
  parseJSON o@(Object v) = do
    d <- v .: (T.pack . symbolVal $ Proxy @(NamedDependency d))
    ds <- parseJSON o
    return $ d :&: ds
  parseJSON _  = fail "Failed to parse a dependency."

instance ( FromJSON (DependencyList Identity ds ds)
         , FromJSON a
         ) => FromJSON (SideLoaded a ds) where
  parseJSON (Object v') = do
        a <- v' .: "data"
        ds <- v' .: "dependencies"
        return $ SideLoaded a ds
  parseJSON _ = fail "Could not parse dependencies."

--------------------------------------------------------------------------------
-- HList Accessors
--------------------------------------------------------------------------------

-- | Project dependency in an accessor type class for HLists. If 'b' in 'bs', type
-- inference is used to project to 'b'.
--
-- > let (SideLoaded a deps) = s
-- > personId . projectDependency $ deps :: PersonId

class ProjectDependency bs b where
  projectDependency :: forall fs m . DependencyList m bs fs -> b

instance {-# OVERLAPPING #-} ProjectDependency (b : bs) b where
  projectDependency (b :&: _) = b

instance {-# OVERLAPPABLE #-} ProjectDependency bs b =>  ProjectDependency (a : bs) b where
  projectDependency (_ :&: bs ) = projectDependency bs

--------------------------------------------------------------------------------
-- Dependent Client
--------------------------------------------------------------------------------

-- | 'DependentClient' is a wrapper around a dependently typed function that when
-- given a singleton 'STrue' returns 'SideLoaded' @a deps@ and when given 'SFalse'
-- returns @a@.
--
-- > data Person =
-- >   Person { personId   :: PersonId
-- >          , personName :: String
-- >          } deriving (Eq, Show, Generic)
-- >
-- > instance FromJSON Person
-- >
-- > data Photo =
-- >   Photo { photoId      :: PhotoId
-- >         , photoCaption :: String
-- >         , artistId     :: PersonId
-- >         } deriving (Eq, Show, Generic)
-- >
-- > instance FromJSON Photo
-- >
-- > data Album =
-- >   Album { albumId     :: AlbumId
-- >         , albumName   :: String
-- >         , albumOwner  :: PersonId
-- >         , albumPhotos :: [PhotoId]
-- >         } deriving (Eq, Show, Generic)
-- >
-- > instance FromJSON Album
-- >
-- > type API = "albums" :> Capture "albumId" AlbumId
-- >                     :> Get '[JSON, PlainText] Album
-- >                     :> SideLoad '[Person, [Photo]]
-- >
-- > type AlbumDeps =  '[Person, [Photo]]
-- >
-- > getAlbumClientFull :: Manager
-- >                    -> BaseUrl
-- >                    -> AlbumId
-- >                    -> IO (Either ServantError (SideLoaded Album AlbumDeps))
-- > getAlbumClientFull m burl aid =
-- >   flip runClientM (ClientEnv m burl) $
-- >     runDepClient (client api aid) STrue
-- >
-- > getAlbumClient :: Manager
-- >                -> BaseUrl
-- >                -> AlbumId
-- >                -> IO (Either ServantError Album)
-- > getAlbumClient m burl aid =
-- >   flip runClientM (ClientEnv m burl) $
-- >     runDepClient (client api aid) SFalse

newtype DepClient (ix :: Bool -> *) (f :: Bool ~> Type) =
    DepClient {runDepClient :: forall (b :: Bool) . ix b -> Client (Apply f b)}

data SideLoadTerminal :: method -> status -> cts -> a -> deps -> (Bool ~> Type) where
  SideLoadTerminal :: SideLoadTerminal method status cts a deps b

type instance Apply (SideLoadTerminal method status cts a deps) b =
  If b (Verb method status cts (SideLoaded a deps)) (Verb method status cts a)

instance {-# OVERLAPPABLE #-}
         ( MimeUnrender JSON a
         , MimeUnrender JSON (SideLoaded a deps)
         , ReflectMethod method
         ) => HasClient (Verb method status cts a :> SideLoad deps) where

  type Client (Verb method status cts a :> SideLoad deps) = DepClient SBool (SideLoadTerminal method status cts a deps)
  clientWithRoute Proxy req = DepClient $ \sb ->
    case sb of
      STrue -> let req' = appendToQueryString "sideload" (Just "true") req
               in snd <$> performRequestCT (Proxy @JSON) method req'
      SFalse -> snd <$> performRequestCT (Proxy @JSON) method req
    where method = reflectMethod (Proxy @method)
