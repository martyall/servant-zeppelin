{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Servant.Zeppelin.Server.Internal where

import Data.Maybe
import Data.Proxy
import Data.Singletons.TypeLits
import Data.List (lookup)
import Servant.Server.Internal
import Servant.API
import Servant.API.ContentTypes
import Servant.Utils.Enter
import Network.Wai (Request, queryString, requestHeaders)
import Network.HTTP.Types

import Servant.Zeppelin
import Servant.Zeppelin.Server.Internal.Zeppelin
import Servant.Zeppelin.Server.Internal.Types

--------------------------------------------------------------------------------
-- | Router
--------------------------------------------------------------------------------

-- | Bind an action after the handler has run
bindAction :: Delayed env (Handler a)
           -> m :~> Handler
           -> (a -> m b)
           -> Delayed env (Handler b)
bindAction Delayed{..} phi f =
  Delayed
    { serverD = \c p a b r -> case serverD c p a b r of
        Route m -> Route $ m >>= (($$) phi) . f
        Fail e -> Fail e
        FailFatal e -> FailFatal e
    , ..
    }

-- | This is just copied from the HasServer instance for QueryFlag.
parseSideLoadedParam :: Request -> Bool
parseSideLoadedParam r =
  let paramname = "sideload"
  in case lookup paramname (queryString r) of
        Just Nothing  -> True  -- param is there, with no value
        Just (Just v) -> examine v -- param with a value
        Nothing       -> False -- param not in the query string
  where
        examine v | v == "true" || v == "1" || v == "" = True
                  | otherwise = False

-- | Inspect the query params for a sideloaded query flag, if true or merely present,
-- inflate the data's dependencies, otherwise just return the data.
methodRouterSideLoad :: ( AllCTRender ctypes (SideLoaded a deps)
                        , HasDependencies m a deps
                        , bs ~ DependencyBase a
                        , CanInflate m bs deps
                        , ToHandler m
                        , Monad m
                        )
                     => Proxy deps
                     -> Proxy m
                     -> m :~> Handler
                     -> Method -> Proxy ctypes -> Status
                     -> Delayed env (Handler a)
                     -> Router env
methodRouterSideLoad _ _ nat method proxy status action =
  leafRouter $ \env request respond ->
    let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
        shouldInflate = parseSideLoadedParam request
        inflationAction = if shouldInflate then inflate else noInflate
    in runAction (bindAction action nat inflationAction
                         `addMethodCheck` methodCheck method request
                         `addAcceptCheck ` acceptCheck proxy accH
                 ) env request respond $ \ output -> do
         let handleA = handleAcceptH proxy (AcceptHeader accH) output
         processMethodRouter handleA status method Nothing request

--------------------------------------------------------------------------------
-- | HasServer instance
--------------------------------------------------------------------------------

instance ( ReflectMethod method, KnownNat status
         , AllCTRender ctypes (SideLoaded a deps)
         , HasDependencies m a deps
         , bs ~ DependencyBase a
         , CanInflate m bs deps
         , ToHandler m
         , HasContextEntry context (m :~> Handler)
         , Monad m
         ) => HasServer (Verb method status ctypes a :> SideLoad deps) context where

  type ServerT (Verb method status ctypes a :> SideLoad deps) m = m a

  route Proxy context = methodRouterSideLoad (Proxy @deps) (Proxy @m) phi method (Proxy @ctypes) status
    where method = reflectMethod (Proxy @method)
          status = toEnum . fromInteger $ natVal (Proxy @status)
          phi = getContextEntry context
