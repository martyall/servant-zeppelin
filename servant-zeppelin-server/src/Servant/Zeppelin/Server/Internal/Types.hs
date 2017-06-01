module Servant.Zeppelin.Server.Internal.Types where

import           Data.Kind
import           Data.Proxy

import           Servant.Zeppelin.Types

--------------------------------------------------------------------------------
-- | Inflators
--------------------------------------------------------------------------------

data Inflators :: (* -> *) -> [*] -> [*] -> * where
  NilInflators :: Inflators m '[] '[]
  (:^) :: (a -> m b) -> Inflators m as bs -> Inflators m (a : as) (b : bs)


class CanInflate m bs fs | m bs -> fs where
  getInflators :: Proxy m -> Proxy bs -> Inflators m bs fs

instance {-# OVERLAPS #-}
         ( Inflatable m b
         , Full m b ~ f
         ) => CanInflate m '[b] '[f] where
  getInflators _ _ = inflator :^ NilInflators

instance {-# OVERLAPPABLE #-}
         ( Inflatable m b
         , Full m b ~ f
         , CanInflate m (b1 ': bs) (f1 ': fs)
         ) => CanInflate m (b ': b1 ': bs) (f ': f1 ': fs) where
  getInflators pm _ = inflator :^ getInflators pm (Proxy @(b1 ': bs))
