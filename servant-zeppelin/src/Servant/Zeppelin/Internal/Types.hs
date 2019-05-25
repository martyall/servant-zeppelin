{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Servant.Zeppelin.Internal.Types
  ( DependencyList(..)
  , NamedDependency
  , SideLoaded(..)
  , Inflatable(..)
  , HasDependencies(..)
  , AllSatisfy
  , Inflatable'
  , Full'
  ) where

import           Data.Functor.Identity   (Identity)
import           Data.Kind
import           Data.Singletons.Prelude hiding (type (*))

--------------------------------------------------------------------------------
-- Dependency Lists
--------------------------------------------------------------------------------

-- | 'DependencyList' @m bs fs@ is a type representing a heterogeneous list parameterized
-- by @bs@ , which can be transformed into a hetergeneous list of type @fs@ in the context
-- provided by @m@.
data DependencyList :: (* -> *) -> [*] -> [*] -> * where
  NilDeps :: DependencyList m '[] '[]
  (:&:) :: b -> DependencyList m bs fs -> DependencyList m (b:bs) (f:fs)

infixr 5 :&:

instance AllSatisfy bs Show' => Show (DependencyList m bs fs) where
  show NilDeps    = "NilDeps"
  show (b :&: bs) = show b ++ " :&: " ++ show bs

instance AllSatisfy bs Eq' => Eq (DependencyList m bs fs) where
  NilDeps == NilDeps = True
  (b :&: bs) == (b' :&: bs') = b == b' && bs == bs'

-- | Labels for the objects created in the dependency mapping. Necessary for JSON instances.
--
-- > type instance NamedDependency Person = "person"
-- > type instance NamedDependency [Photo] = "photos"
--
type family NamedDependency a :: Symbol

-- | 'SideLoaded' @a deps@ represents a type @a@ with an hlist of its inflated dependencies.
data SideLoaded a (deps :: [*]) = SideLoaded a (DependencyList Identity deps deps)

deriving instance (Show a, Show (DependencyList Identity deps deps)) => Show (SideLoaded a deps)
deriving instance (Eq a, Eq (DependencyList Identity deps deps)) => Eq (SideLoaded a deps)

-- | Inflatable represents a type 'b' which can be expanded inside of a context 'm'.
--
-- > type PGMonad = ReaderT Connection (ExceptT QueryError IO)
-- >
-- > instance Inflatable PGMonad PersonId where
-- >   type Full PGMonad PersonId = Person
-- >   inflator = getPersonById

class Inflatable m base where
  type Full m base :: *
  inflator :: base -> m (Full m base)

-- | Anything can be expanded into itself in the trivial context.
instance Inflatable Identity base where
  type Full Identity base = base
  inflator = return

--------------------------------------------------------------------------------
-- HasDepedencies
--------------------------------------------------------------------------------

-- | Indicate that a type has dependencies, and supply the uninflated values.
--
-- > data Album =
-- >   Album { albumId     :: AlbumId
-- >         , albumArtist :: PersonId
-- >         , albumPhotos :: [PhotoId]
-- >         , albumTitle  :: Text
-- >         }
-- >
-- > instance HasDependencies PGMonad Album '[Person, [PhotoId]] where
-- >   getDependencies album = albumArtist album :&: albumPhotos album :&: NilDeps

class AllSatisfy bs (Inflatable' m) => HasDependencies m a bs | a -> bs, bs -> m where
  getDependencies :: a -> DependencyList m bs (Map (Full' m) bs)

--------------------------------------------------------------------------------
-- Type Families
--------------------------------------------------------------------------------

-- | All subjects must satisfy the test constraint.
type family AllSatisfy (subjects :: [k]) (test :: (k ~> Constraint)) :: Constraint where
  AllSatisfy '[] test = ()
  AllSatisfy (subj : rest) test = (Apply test subj, AllSatisfy rest test)

-- | Parially applied 'Inflatable' constraint.
data Inflatable' :: m -> (base ~> Constraint) where
  Inflatable' :: Inflatable' m base

type instance Apply (Inflatable' m) base = Inflatable m base

-- | Defunctionalized 'Full' type family to be used with 'Map'.
data Full' :: m -> (base ~> *) where
  Full' :: Full' m base

type instance Apply (Full' m) base = Full m base

data Eq' :: b ~> Constraint where
  Eq' :: Eq' b

type instance Apply Eq' b = Eq b

data Show' :: b ~> Constraint where
  Show' :: Show' b

type instance Apply Show' b = Show b
