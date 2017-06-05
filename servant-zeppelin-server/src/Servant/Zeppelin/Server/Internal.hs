{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}

module Servant.Zeppelin.Server.Internal where

import           Data.List                                 (lookup)
import           Data.Maybe
import           Data.Proxy
import           Data.Singletons.Prelude                   hiding ((:>))
import           Data.Singletons.TypeLits
import           Network.HTTP.Types
import           Network.Wai                               (Request,
                                                            queryString,
                                                            requestHeaders)
import           Servant.API
import           Servant.API.ContentTypes
import           Servant.Server.Internal
import           Servant.Utils.Enter

import           Servant.Zeppelin
import           Servant.Zeppelin.Server.Internal.Types
import           Servant.Zeppelin.Server.Internal.Zeppelin
import           Servant.Zeppelin.Internal.Types

--------------------------------------------------------------------------------
-- Router
--------------------------------------------------------------------------------

-- | Bind an action after the handler has run
bindAction :: Delayed env (Handler a)
           -> t :~> Handler
           -> (a -> t b)
           -> Delayed env (Handler b)
bindAction Delayed{..} phi f =
  Delayed
    { serverD = \c p h a b r -> case serverD c p h a b r of
        Route m     -> Route $ m >>= ($$) phi . f
        Fail e      -> Fail e
        FailFatal e -> FailFatal e
    , ..
    }

-- | This is just copied from the HasServer instance for QueryFlag.
checkSideLoadedParam :: Request -> Bool
checkSideLoadedParam r =
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
methodRouterSideLoad :: ( AllCTRender ctypes (SideLoaded a fs)
                        , HasDependencies t a bs
                        , fs ~ Map (Full' t) bs
                        , CanInflate t bs fs
                        , AllCTRender ctypes a
                        , Monad t
                        )
                     => Proxy t
                     -> Proxy bs
                     -> t :~> Handler
                     -> Method -> Proxy ctypes -> Status
                     -> Delayed env (Handler a)
                     -> Router env
methodRouterSideLoad pm pdeps nat method proxy status action =
  leafRouter $ \env request respond ->
    let accH = fromMaybe ct_wildcard $ lookup hAccept $ requestHeaders request
        shouldInflate = checkSideLoadedParam request
    in if shouldInflate
         then runAction (bindAction action nat (inflate pm pdeps)
                          `addMethodCheck` methodCheck method request
                          `addAcceptCheck ` acceptCheck proxy accH
                        ) env request respond $ \ output -> do
           let handleA = handleAcceptH proxy (AcceptHeader accH) output
           processMethodRouter handleA status method Nothing request
         else runAction (action `addMethodCheck` methodCheck method request
                          `addAcceptCheck ` acceptCheck proxy accH
                        ) env request respond $ \ output -> do
           let handleA = handleAcceptH proxy (AcceptHeader accH) output
           processMethodRouter handleA status method Nothing request

--------------------------------------------------------------------------------
-- HasServer instance
--------------------------------------------------------------------------------

instance ( ReflectMethod method, KnownNat status
         , CanInflate t bs fs
         , HasDependencies t a bs
         , fs ~ Map (Full' t) bs
         , AllCTRender ctypes (SideLoaded a fs)
         , AllCTRender ctypes a
         , Monad t
         , HasContextEntry context (t :~> Handler)
         ) => HasServer (Verb method status ctypes a :> SideLoad fs) context where

  type ServerT (Verb method status ctypes a :> SideLoad fs) m = m a

  route Proxy context = methodRouterSideLoad (Proxy @t) (Proxy @bs) phi method (Proxy @ctypes) status
    where method = reflectMethod (Proxy @method)
          status = toEnum . fromInteger $ natVal (Proxy @status)
          phi = getContextEntry context
