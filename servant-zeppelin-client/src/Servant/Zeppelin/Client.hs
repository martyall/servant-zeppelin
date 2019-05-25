{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Zeppelin.Client
  ( projectDependency
  , DepClient(..)
  -- * Re-exports
  , SideLoaded(..)
  , SideLoad
  , SBool
  ) where

import           Data.Aeson
import           Data.Functor.Identity
import           Data.Kind
import           Data.Proxy
import           Data.Singletons.Prelude hiding (type (*))
import           Data.Singletons.TypeLits
import qualified Data.Text                as T
import           Servant.API             hiding (SBool(..))
import           Servant.Client
import Servant.Client.Core (appendToQueryString)

import           Servant.Client.Core.Internal.Request
import           Servant.Client.Core.Internal.RunClient (RunClient, runRequest, decodedAs)
import           Data.Foldable                          (toList)
import           Data.Sequence                          (fromList)

import           Servant.Zeppelin
import           Servant.Zeppelin.Internal.Types

--------------------------------------------------------------------------------
-- FromJSON Instances
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

-- | 'ProjectDependency' @bs b' allows you to project to a dependency type from
-- a 'DependencyList'. If 'b' in 'bs', type inference is used to project to 'b'.
-- For example:
--
-- > let (SideLoaded a deps) = s :: SideLoaded Album '[Person, [Photo]]
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
-- given a singleton 'STrue' has return type 'SideLoaded' @a deps@, and
-- when given 'SFalse' has return type @a@. For example:
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

newtype DepClient (ix :: Bool -> *) (f :: Bool ~> Type) (m :: * -> *)  =
    DepClient {runDepClient :: forall (b :: Bool) . ix b -> Client m (Apply f b)}

data SideLoadTerminal :: method -> status -> cts -> a -> deps -> (Bool ~> Type) where
  SideLoadTerminal :: SideLoadTerminal method status cts a deps b

type instance Apply (SideLoadTerminal method status cts a deps) b =
  If b (Verb method status cts (SideLoaded a deps)) (Verb method status cts a)

instance {-# OVERLAPPABLE #-}
         ( MimeUnrender JSON a
         , MimeUnrender JSON (SideLoaded a deps)
         , ReflectMethod method
         , RunClient m
         ) => HasClient m (Verb method status cts a :> SideLoad deps) where

  type Client m (Verb method status cts a :> SideLoad deps) = DepClient SBool (SideLoadTerminal method status cts a deps) m
 -- hoistClientMonad _ _ f ma = f ma
  clientWithRoute _ _ req = DepClient $ \sb ->
    case sb of
      STrue -> do
        let req' = appendToQueryString "sideload" (Just "true") req
        response <- runRequest req' { requestAccept = fromList $ toList accept
                                    , requestMethod = method
                                    }
        response `decodedAs` (Proxy :: Proxy JSON)

      SFalse -> do
        response <- runRequest req { requestAccept = fromList $ toList accept
                                   , requestMethod = method
                                   }
        response `decodedAs` (Proxy :: Proxy JSON)
    where
      accept = contentTypes (Proxy :: Proxy JSON)
      method = reflectMethod (Proxy @method)
