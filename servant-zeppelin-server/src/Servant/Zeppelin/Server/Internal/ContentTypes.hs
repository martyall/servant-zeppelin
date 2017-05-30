{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Zeppelin.Server.Internal.ContentTypes where

import           Data.Aeson
import           Data.Functor.Identity
import           Data.Proxy
import           Data.Singletons.TypeLits
import           Data.Text                              (Text, pack)

import           Servant.Zeppelin.Server.Internal.Types
import           Servant.Zeppelin.Types

--------------------------------------------------------------------------------
-- | JSON Instances
--------------------------------------------------------------------------------

-- | A helper class for the JSON instances
class ToKeyValueList a where
  toKeyValueList :: a -> [(Text, Value)]

instance ToKeyValueList (DependencyList Identity '[] '[]) where
  toKeyValueList _ = []

instance ( ToJSON d
         , KnownSymbol (NamedDependency d)
         , ToKeyValueList (DependencyList Identity ds ds)
         , n ~ 'S (Length ds)
         ) => ToKeyValueList (DependencyList Identity (d:ds) (d:ds)) where
  toKeyValueList IgnoreDeps = []
  toKeyValueList (a :&: rest) =
    let k = pack . symbolVal $ Proxy @(NamedDependency d)
        v = toJSON a
    in (k, v) : toKeyValueList rest

instance ToKeyValueList (DependencyList Identity ds ds) => ToJSON (DependencyList Identity ds ds) where
  toJSON ds = object $ toKeyValueList ds

instance {-# OVERLAPPABLE #-}
         ( ToJSON (DependencyList Identity deps deps)
         , ToJSON a
         ) => ToJSON (SideLoaded a deps) where
  toJSON (SideLoaded _data deps) =
    case deps of
      IgnoreDeps -> toJSON _data
      deps' -> object [ "data" .= toJSON _data
                      , "dependencies" .= toJSON deps'
                      ]
