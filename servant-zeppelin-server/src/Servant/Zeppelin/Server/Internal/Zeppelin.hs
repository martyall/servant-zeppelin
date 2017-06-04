module Servant.Zeppelin.Server.Internal.Zeppelin where

import           Data.Functor.Identity                  (Identity (..))
import           Data.Proxy
import           Data.Singletons.Prelude
import           Servant.Zeppelin.Server.Internal.Types
import           Servant.Zeppelin.Types

----------------------------------------------------------------------------------
-- Side Loading
----------------------------------------------------------------------------------

-- | Monadically sequence the inflations.
sequenceDependencyList :: Monad m
                       => DependencyList m bs fs
                       -> Inflators m bs fs
                       -> m (DependencyList Identity fs fs)
sequenceDependencyList NilDeps NilInflators = return NilDeps
sequenceDependencyList (b :&: bs) (i :^ is) = do
  f <- i b
  fs <- sequenceDependencyList bs is
  return $ f :&: fs

-- | Run the inflators and wrap in the SideLoaded type.
inflate :: ( HasDependencies m a bs
           , fs ~ Map (Full' m) bs
           , CanInflate m bs fs
           , Monad m
           )
        => Proxy m
        -> Proxy bs
        -> a
        -> m (SideLoaded a fs)
inflate pm pbs value =
  let dependencies = getDependencies value
      inflators = getInflators pm pbs
  in sequenceDependencyList dependencies inflators >>= \deps -> return $ SideLoaded value deps
