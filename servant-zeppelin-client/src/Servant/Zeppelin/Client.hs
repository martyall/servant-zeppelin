{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ExplicitNamespaces   #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Zeppelin.Client where

import           Data.Aeson
import           Data.Functor.Identity
import           Data.Kind
import           Data.Proxy
import           Data.Singletons.Prelude  (Apply, If, type (~>))
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
-- | HList Accessors
--------------------------------------------------------------------------------

class ProjectDependency bs b where
  projectDependency :: forall fs m . DependencyList m bs fs -> b

instance {-# OVERLAPPING #-} ProjectDependency (b : bs) b where
  projectDependency (b :&: _) = b

instance {-# OVERLAPPABLE #-} ProjectDependency bs b =>  ProjectDependency (a : bs) b where
  projectDependency (_ :&: bs ) = projectDependency bs

--------------------------------------------------------------------------------
-- | Dependent Client
--------------------------------------------------------------------------------

data SBool :: Bool -> * where
  STrue :: SBool 'True
  SFalse :: SBool 'False

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
