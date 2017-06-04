module Servant.Zeppelin.Server.Internal.Types where

import           Data.Kind
import           Data.Proxy

import           Servant.Zeppelin.Internal.Types

--------------------------------------------------------------------------------
-- Inflators
--------------------------------------------------------------------------------

-- | 'Inflators' @m as bs@ represents a heterogeneous list of Kliesli arrows that
-- come from instances for 'Inflatable' @m a@ for all @a@ in @as@.
data Inflators :: (* -> *) -> [*] -> [*] -> * where
  NilInflators :: Inflators m '[] '[]
  (:^) :: (a -> m b) -> Inflators m as bs -> Inflators m (a : as) (b : bs)

-- | Because of the functional dependency on 'Inflatable' @m a@, it is sufficient
-- to supply only the proxy types to get the inflators.
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
