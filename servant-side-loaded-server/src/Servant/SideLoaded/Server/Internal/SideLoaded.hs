module Servant.SideLoaded.Server.Internal.SideLoaded
    ( NamedDependency
    , Inflatable(..)
    , DependencyList(NilDeps)
    , HasDependencies(..)
    , SideLoaded
    , inflate
    , (&:)
    ) where

import Data.Aeson (ToJSON(..), Value, (.=), object)
import Data.Kind
import Data.Proxy
import Data.Functor.Identity (Identity(..))
import Data.Singletons (Apply, type (~>))
import Data.Text (Text, pack)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

--------------------------------------------------------------------------------
-- | Dependency Lists
--------------------------------------------------------------------------------

data DependencyList :: (* -> *) -> [*] -> [*] -> * where
  NilDeps :: DependencyList m '[] '[]
  (:&:) :: Inflatable m b f => b -> DependencyList m bs fs -> DependencyList m (b:bs) (f:fs)

(&:) :: ( Monad m
        , Inflatable m b f
        )
     => b
     -> DependencyList m bs fs
     -> DependencyList m (b:bs) (f:fs)
(&:) b rest = b :&: rest

infixr 5 &:

-- | Labels for the objects created in the dependency mapping. See JSON instances.
type family NamedDependency (a :: Type) :: Symbol

--------------------------------------------------------------------------------
-- | Inflatables
--------------------------------------------------------------------------------

-- | Inflatable represents an entity which can be expanded inside of a context @m@.
class Inflatable m base full | base m -> full where
  inflator :: base -> m full

-- | Anything can be expanded into itself in the trivial context
instance Inflatable Identity base base where
  inflator = return

-- | Indicate that a type has dependencies, and supply the uninflated types
-- (order matters here).
class HasDependencies m a full | a -> full where
  type DependencyBase a :: [*]
  getDependencies :: a -> DependencyList m (DependencyBase a) full

----------------------------------------------------------------------------------
---- | Side Loading
----------------------------------------------------------------------------------

-- | Run the inflator in a monadic sequence.
sequenceDependencyList :: Monad m => DependencyList m bs fs -> m (DependencyList Identity fs fs)
sequenceDependencyList NilDeps = return NilDeps
sequenceDependencyList (b :&: rest) = do
  f <- inflator b
  fs <- sequenceDependencyList rest
  return (f :&: fs)

data SideLoaded a (deps :: [*]) = SideLoaded a (DependencyList Identity deps deps)

-- | Run the inflators and wrap in the SideLoaded type.
inflate :: ( HasDependencies m a fs
           , bs ~ DependencyBase a
           , CanInflate m bs fs
           , Monad m
           )
        => a
        -> m (SideLoaded a fs)
inflate value =
  let dependencies = getDependencies value
  in sequenceDependencyList dependencies >>= \deps -> return $ SideLoaded value deps

--------------------------------------------------------------------------------
-- | JSON Instances
--------------------------------------------------------------------------------

class ToKeyValueList a where
  toKeyValueList :: a -> [(Text, Value)]

instance ToKeyValueList (DependencyList Identity '[] '[]) where
  toKeyValueList _ = []

instance ( ToJSON d
         , KnownSymbol (NamedDependency d)
         , ToKeyValueList (DependencyList Identity ds ds)
         ) => ToKeyValueList (DependencyList Identity (d:ds) (d:ds)) where
  toKeyValueList (a :&: rest) =
    let k = pack . symbolVal $ Proxy @(NamedDependency d)
        v = toJSON a
    in (k, v) : toKeyValueList rest

instance ToKeyValueList (DependencyList Identity ds ds) => ToJSON (DependencyList Identity ds ds) where
  toJSON ds = object $ toKeyValueList ds

instance ( ToJSON (DependencyList Identity deps deps)
         , ToJSON a
         ) => ToJSON (SideLoaded a deps) where
  toJSON (SideLoaded _data deps) = object [ "data" .= toJSON _data
                                          , "dependencies" .= toJSON deps
                                          ]
----------------------------------------------------------------------------------
---- Type Families
----------------------------------------------------------------------------------

type family AllSatisfy (subjects :: [k]) (test :: (k ~> Constraint)) :: Constraint where
  AllSatisfy '[] test = ()
  AllSatisfy (subj : rest) test = (Apply test subj, AllSatisfy rest test)

type family CanInflate (m :: * -> *) (bs :: [*]) (fs :: [*]) :: Constraint where
  CanInflate m '[] '[] = ()
  CanInflate m (b:bs) (f:fs) = (Inflatable m b f, CanInflate m bs fs)
