{-# LANGUAGE RecordWildCards #-}

module Servant.SideLoaded.Server.Internal where
import Network.HTTP.Types
import           Network.Wai                (requestHeaders)
import Data.CaseInsensitive (mk)
import qualified Data.List as L
import Data.Proxy
import Data.Maybe
import Data.String.Conversions
import Text.Read (readMaybe)
import Servant.SideLoaded.Server.Internal.SideLoaded
import Servant.Server.Internal
import Servant.API.ContentTypes
import Servant.Utils.Enter


actionBind :: forall m env a b. Monad m
           => Delayed env (m a)
           -> (m :~> Handler)
           -> (a -> m b)
           -> Delayed env (Handler b)
actionBind Delayed{..} phi f =
  Delayed
    { serverD = \c p a b r -> case serverD c p a b r of
        Fail e -> Fail e
        FailFatal e -> FailFatal e
        Route mx -> Route $ phi $$ (mx >>= f)
    , ..
    }

methodRouterSideloaded :: ( AllCTRender ctypes a
                          , AllCTRender ctypes (SideLoaded a fs)
                          , HasDependencies m a fs
                          , bs ~ DependencyBase a
                          , CanInflate m bs fs
                          , Monad m
                          )
                       => Method -> Proxy ctypes -> Status
                       -> Delayed env (m a)
                       -> (m :~> Handler)
                       -> Router env
methodRouterSideloaded method proxy status action phi = leafRouter route'
  where
    route' env request respond =
      let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
          sideloadH = fromMaybe False $ do
            bSideLoad <- L.lookup (mk "X-Sideloaded") $ requestHeaders request
            readMaybe . cs $ bSideLoad

      in case sideloadH of
          False ->
            let action' = actionBind action phi return
            in runAction (action' `addMethodCheck` methodCheck method request
                                  `addAcceptCheck` acceptCheck proxy accH
                         ) env request respond $ \ output -> do
                 let handleA = handleAcceptH proxy (AcceptHeader accH) output
                 processMethodRouter handleA status method Nothing request

          True ->
            let action' = actionBind action phi inflate
            in runAction (action' `addMethodCheck` methodCheck method request
                               `addAcceptCheck` acceptCheck proxy accH
                         ) env request respond $ \ (output :: SideLoaded a deps) -> do
              let handleA = handleAcceptH proxy (AcceptHeader accH) output
              processMethodRouter handleA status method Nothing request
