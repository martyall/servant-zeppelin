{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Servant.Zeppelin.Types where

import           Data.Functor.Identity   (Identity)
import           Data.Kind
import           Data.Singletons.Prelude

--------------------------------------------------------------------------------
-- | Dependency Lists
--------------------------------------------------------------------------------

data DependencyList :: (* -> *) -> [*] -> [*] -> * where
  NilDeps :: DependencyList m '[] '[]
  (:&:) :: b -> DependencyList m bs fs -> DependencyList m (b:bs) (f:fs)

(&:) :: ( Monad m
        , Inflatable m b
        , Full m b ~ f
        )
     => b
     -> DependencyList m bs fs
     -> DependencyList m (b:bs) (f:fs)
(&:) b rest = b :&: rest

infixr 5 &:

instance AllSatisfy bs Show' => Show (DependencyList m bs fs) where
  show NilDeps    = "NilDeps"
  show (b :&: bs) = show b ++ " :&: " ++ show bs

instance AllSatisfy bs Eq' => Eq (DependencyList m bs fs) where
  NilDeps == NilDeps = True
  (b :&: bs) == (b' :&: bs') = b == b' && bs == bs'

-- | Labels for the objects created in the dependency mapping. Useful for JSON instances.
type family NamedDependency (a :: Type) :: Symbol

data SideLoaded a (deps :: [*]) = SideLoaded a (DependencyList Identity deps deps)

deriving instance (Show a, Show (DependencyList Identity deps deps)) => Show (SideLoaded a deps)
deriving instance (Eq a, Eq (DependencyList Identity deps deps)) => Eq (SideLoaded a deps)

-- | Inflatable represents an entity which can be expanded inside of a context @m@.
class Inflatable m base where
  type Full m base :: k
  inflator :: base -> m (Full m base)

-- | Anything can be expanded into itself in the trivial context
instance Inflatable Identity base where
  type Full Identity base = base
  inflator = return

--------------------------------------------------------------------------------
-- | HasDepedencies
--------------------------------------------------------------------------------

-- | Indicate that a type has dependencies, and supply the uninflated types
-- (order matters here).
class AllSatisfy bs (Inflatable' m) => HasDependencies m a bs | a -> bs, bs -> m where
  getDependencies :: a -> DependencyList m bs (Map (Full' m) bs)

--------------------------------------------------------------------------------
-- Type Families
--------------------------------------------------------------------------------

type family AllSatisfy (subjects :: [k]) (test :: (k ~> Constraint)) :: Constraint where
  AllSatisfy '[] test = ()
  AllSatisfy (subj : rest) test = (Apply test subj, AllSatisfy rest test)

data Inflatable' :: m -> (base ~> Constraint) where
  Inflatable' :: Inflatable' m base

type instance Apply (Inflatable' m) base = Inflatable m base

data Full' :: m -> (base ~> Type) where
  Full' :: Full' m base

type instance Apply (Full' m) base = Full m base

data Eq' :: b ~> Constraint where
  Eq' :: Eq' b

type instance Apply Eq' b = Eq b

data Show' :: b ~> Constraint where
  Show' :: Show' b

type instance Apply Show' b = Show b
