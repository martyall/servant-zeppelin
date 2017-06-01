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

data Nat = Z | S Nat

class (n ~ Length fs) => CanInflate m n bs fs | m bs -> fs where
  getInflators :: Proxy m -> Proxy bs -> Inflators m bs fs

instance {-# OVERLAPS #-}
         ( Inflatable m b
         , Full m b ~ f
         ) => CanInflate m ('S 'Z) '[b] '[f] where
  getInflators _ _ = inflator :^ NilInflators

instance {-# OVERLAPPABLE #-}
         ( Inflatable m b
         , Full m b ~ f
         , CanInflate m n bs fs
         ) => CanInflate m ('S n) (b : bs) (f : fs) where
  getInflators pm _ = inflator :^ getInflators pm (Proxy @bs)

--------------------------------------------------------------------------------
-- Type Families
--------------------------------------------------------------------------------

type family Length (as :: [*]) :: Nat where
  Length '[] = 'Z
  Length (a:as) = 'S (Length as)
