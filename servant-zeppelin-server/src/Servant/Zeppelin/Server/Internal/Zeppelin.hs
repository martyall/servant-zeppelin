module Servant.Zeppelin.Server.Internal.Zeppelin where

import Data.Proxy
import Data.Functor.Identity (Identity(..))
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

sequenceDependencyList :: Monad m
                       => DependencyList m bs fs
                       -> Inflators m bs fs
                       -> m (DependencyList Identity fs fs)
sequenceDependencyList IgnoreDeps _ = return IgnoreDeps
sequenceDependencyList NilDeps NilInflators = return NilDeps
sequenceDependencyList (b :&: bs) (i :^ is) = do
  f <- i b
  fs <- sequenceDependencyList bs is
  return $ f :&: fs

-- | Run the inflators and wrap in the SideLoaded type.
inflate :: ( HasDependencies m a fs
           , bs ~ DependencyBase a
           , CanInflate m n bs fs
           , Monad m
           )
        => Proxy m
        -> Proxy fs
        -> a
        -> m (SideLoaded a fs)
inflate pm pfs value =
  let dependencies = getDependencies value
      inflators = getInflators pm pfs
  in sequenceDependencyList dependencies inflators >>= \deps -> return $ SideLoaded value deps

-- | Ignore the inflation step.
noInflate :: forall m fs a . Monad m => a -> m (SideLoaded a fs)
noInflate a = return $ SideLoaded a IgnoreDeps
