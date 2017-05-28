module Servant.Zeppelin where

import Data.Functor.Identity
import Data.Singletons.TypeLits

data SideLoad (s :: Symbol)

data DependencyList :: (* -> *) -> [*] -> [*] -> * where
  NilDeps :: DependencyList m '[] '[]
  IgnoreDeps :: DependencyList m as bs
  (:&:) :: b -> DependencyList m bs fs -> DependencyList m (b:bs) (f:fs)


data SideLoaded a (deps :: [*]) = SideLoaded a (DependencyList Identity deps deps)
