module Servant.Zeppelin.Server.Internal.Types where

import Data.Functor.Identity
import Data.Kind
import Servant.Utils.Enter
import Data.Singletons (Apply, type (~>))
import           Servant.Server.Internal.Handler


import Servant.Zeppelin

--------------------------------------------------------------------------------
-- | Inflatables
--------------------------------------------------------------------------------

-- | Inflatable represents an entity which can be expanded inside of a context @m@.
class Inflatable m base full | base m -> full, full base -> m where
  inflator :: base -> m full

-- | Anything can be expanded into itself in the trivial context
instance Inflatable Identity base base where
  inflator = return

-- | Indicate that a type has dependencies, and supply the uninflated types
-- (order matters here).
class HasDependencies m a fs | a -> fs, a fs -> m where
  type DependencyBase a :: [*]
  getDependencies :: a -> DependencyList m (DependencyBase a) fs

--------------------------------------------------------------------------------
-- | Servant Handler
--------------------------------------------------------------------------------

-- | An instance of ToHandler must be preset in order to integrate with the
-- servant router.

class ToHandler m where
  toServantHandler :: m :~> Handler

instance ToHandler Handler where
  toServantHandler = NT id

----------------------------------------------------------------------------------
---- Type Families
----------------------------------------------------------------------------------

type family AllSatisfy (subjects :: [k]) (test :: (k ~> Constraint)) :: Constraint where
  AllSatisfy '[] test = ()
  AllSatisfy (subj : rest) test = (Apply test subj, AllSatisfy rest test)

type family CanInflate (m :: * -> *) (bs :: [*]) (fs :: [*]) :: Constraint where
  CanInflate m '[] '[] = ()
  CanInflate m (b:bs) (f:fs) = (Inflatable m b f, CanInflate m bs fs)

data Inflatable' :: m -> b -> (f ~> Constraint) where
  Inflatable' :: Inflatable' m b f

type instance Apply (Inflatable' m b) f = Inflatable m b f
