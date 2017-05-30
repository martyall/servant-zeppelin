{-# LANGUAGE TypeFamilyDependencies #-}

module Servant.Zeppelin.Server.Internal.Types where

import Data.Functor.Identity
import Data.Kind
import Data.Proxy

--import Data.Constraint
import Data.Singletons (Apply, type (~>))


--------------------------------------------------------------------------------
-- | Dependency Lists
--------------------------------------------------------------------------------

data DependencyList :: (* -> *) -> [*] -> [*] -> * where
  IgnoreDeps :: DependencyList m as bs
  NilDeps :: DependencyList m '[] '[]
  (:&:) :: b -> DependencyList m bs fs -> DependencyList m (b:bs) (f:fs)

data SideLoaded a (deps :: [*]) = SideLoaded a (DependencyList Identity deps deps)

--------------------------------------------------------------------------------
-- | Inflatables
--------------------------------------------------------------------------------

-- | Inflatable represents an entity which can be expanded inside of a context @m@.
class Inflatable m base full | base full -> m, m base -> full, full m -> base where
  inflator :: base -> m full

-- | Anything can be expanded into itself in the trivial context
instance Inflatable Identity base base where
  inflator = return

-- | Indicate that a type has dependencies, and supply the uninflated types
-- (order matters here).
class HasDependencies m a fs | a fs -> m, m a -> fs where
  type DependencyBase a :: [*]
  getDependencies :: a -> DependencyList m (DependencyBase a) fs

--------------------------------------------------------------------------------
-- | Servant Handler
--------------------------------------------------------------------------------

data Inflators :: (* -> *) -> [*] -> [*] -> * where
  NilInflators :: Inflators m '[] '[]
  (:^) :: (a -> m b) -> Inflators m as bs -> Inflators m (a : as) (b : bs)

data Nat = Z | S Nat

class (n ~ Length fs) => CanInflate m n bs fs | bs fs -> m, m bs -> fs, m fs -> bs where
  getInflators :: Proxy m -> Proxy fs -> Inflators m bs fs

instance {-# OVERLAPS #-} Inflatable m b f => CanInflate m ('S 'Z) '[b] '[f] where
  getInflators _ _ = inflator :^ NilInflators

instance {-# OVERLAPPABLE #-}
         ( Inflatable m b f
         , CanInflate m n bs fs
         ) => CanInflate m ('S n) (b : bs) (f : fs) where
  getInflators pm _ = inflator :^ getInflators pm (Proxy @fs)

----------------------------------------------------------------------------------
---- Type Families
----------------------------------------------------------------------------------

type family AllSatisfy (subjects :: [k]) (test :: (k ~> Constraint)) :: Constraint where
  AllSatisfy '[] test = ()
  AllSatisfy (subj : rest) test = (Apply test subj, AllSatisfy rest test)

--type family CanInflate (m :: * -> *) (bs :: [*]) (fs :: [*]) = (c :: Constraint) | bs -> m where
--  CanInflate m '[] '[] = ()
--  CanInflate m (b:bs) (f:fs) = (Inflatable m b f, CanInflate m bs fs)

data Inflatable' :: m -> b -> (f ~> Constraint) where
  Inflatable' :: Inflatable' m b f

type instance Apply (Inflatable' m b) f = Inflatable m b f

type family Length (as :: [*]) :: Nat where
  Length '[] = 'Z
  Length (a:as) = 'S (Length as)
