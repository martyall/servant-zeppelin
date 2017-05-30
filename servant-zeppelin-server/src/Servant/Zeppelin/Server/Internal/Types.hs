{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Servant.Zeppelin.Server.Internal.Types where

import Data.Functor.Identity
import Data.Kind
import Data.Proxy

--import Data.Constraint
import Data.Singletons.Prelude


--------------------------------------------------------------------------------
-- | Dependency Lists
--------------------------------------------------------------------------------

data DependencyList :: (* -> *) -> [*] -> [*] -> * where
  IgnoreDeps :: DependencyList m bs fs
  NilDeps :: DependencyList m '[] '[]
  (:&:) :: b -> DependencyList m bs fs -> DependencyList m (b:bs) (f:fs)

data SideLoaded a (deps :: [*]) = SideLoaded a (DependencyList Identity deps deps)

--------------------------------------------------------------------------------
-- | Inflatables
--------------------------------------------------------------------------------

-- | Inflatable represents an entity which can be expanded inside of a context @m@.
class Inflatable m base where
  type Full m base :: k
  inflator :: base -> m (Full m base)

-- | Anything can be expanded into itself in the trivial context
instance Inflatable Identity base where
  type Full Identity base = base
  inflator = return

-- | Indicate that a type has dependencies, and supply the uninflated types
-- (order matters here).
class AllSatisfy bs (Inflatable' m) => HasDependencies m a bs | a -> bs, bs -> m where
  getDependencies :: a -> DependencyList m bs (Map (Full' m) bs)

----------------------------------------------------------------------------------
---- | Servant Handler
----------------------------------------------------------------------------------

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

----------------------------------------------------------------------------------
---- Type Families
----------------------------------------------------------------------------------

type family AllSatisfy (subjects :: [k]) (test :: (k ~> Constraint)) :: Constraint where
  AllSatisfy '[] test = ()
  AllSatisfy (subj : rest) test = (Apply test subj, AllSatisfy rest test)

data Inflatable' :: m -> (base ~> Constraint) where
  Inflatable' :: Inflatable' m base

type instance Apply (Inflatable' m) base = Inflatable m base

data Full' :: m -> (base ~> Type) where
  Full' :: Full' m base

type instance Apply (Full' m) base = Full m base

type family Length (as :: [*]) :: Nat where
  Length '[] = 'Z
  Length (a:as) = 'S (Length as)
