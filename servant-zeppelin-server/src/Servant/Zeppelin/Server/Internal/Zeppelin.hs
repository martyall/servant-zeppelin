
module Servant.Zeppelin.Server.Internal.Zeppelin where

import Data.Functor.Identity (Identity(..))

import Servant.Zeppelin
import Servant.Zeppelin.Server.Internal.Types

--------------------------------------------------------------------------------
-- | Dependency Lists
--------------------------------------------------------------------------------

(&:) :: ( Monad m
        , Inflatable m b f
        )
     => b
     -> DependencyList m bs fs
     -> DependencyList m (b:bs) (f:fs)
(&:) b rest = b :&: rest

infixr 5 &:

----------------------------------------------------------------------------------
---- | Side Loading
----------------------------------------------------------------------------------

-- | Run the inflator in a monadic sequence.
sequenceDependencyList :: ( Monad m
                          , CanInflate m bs fs
                          )
                       => DependencyList m bs fs
                       -> m (DependencyList Identity fs fs)
sequenceDependencyList IgnoreDeps = return IgnoreDeps
sequenceDependencyList NilDeps = return NilDeps
sequenceDependencyList (b :&: rest) = do
  f <- inflator b
  fs <- sequenceDependencyList rest
  return (f :&: fs)

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

-- | Ignore the inflation step.
noInflate :: forall m fs a . Monad m => a -> m (SideLoaded a fs)
noInflate a = return $ SideLoaded a IgnoreDeps
