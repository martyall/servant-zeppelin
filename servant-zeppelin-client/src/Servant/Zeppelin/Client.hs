{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Zeppelin.Client where

import Data.Aeson
import Data.Functor.Identity
import Data.Proxy
import Data.Kind
import Data.Singletons.Prelude (type (~>), Apply, If)
import Data.Singletons.TypeLits
import qualified Data.Text as T
import           Servant.Client
import           Servant.Common.Req
import           Servant.API

import           Servant.Zeppelin
import           Servant.Zeppelin.Types

instance FromJSON (DependencyList Identity '[] '[]) where
  parseJSON (Object _) = return NilDeps
  -- this can't actually happen.
  parseJSON _ = fail "Nil dependency list should be the empty object."

instance ( FromJSON (DependencyList Identity ds ds)
         , KnownSymbol (NamedDependency d)
         , FromJSON d
         ) => FromJSON (DependencyList Identity (d : ds) (d : ds)) where
  parseJSON o@(Object v) = do
    d <- v .: (T.pack . symbolVal $ Proxy @(NamedDependency d))
    ds <- parseJSON o
    return $ d :&: ds
  parseJSON _  = fail "Failed to parse a dependency."

instance ( FromJSON (DependencyList Identity ds ds)
         , FromJSON a
         ) => FromJSON (SideLoaded a ds) where
  parseJSON (Object v') = do
        a <- v' .: "data"
        ds <- v' .: "dependencies"
        return $ SideLoaded a ds
  parseJSON _ = fail "Could not parse dependencies."

class ProjectDependency bs b where
  projectDependency :: forall fs m . DependencyList m bs fs -> b

instance {-# OVERLAPPING #-} ProjectDependency (b : bs) b where
  projectDependency (b :&: _) = b

instance {-# OVERLAPPABLE #-} ProjectDependency bs b =>  ProjectDependency (a : bs) b where
  projectDependency (_ :&: bs ) = projectDependency bs

data SBool :: Bool -> * where
  STrue :: SBool 'True
  SFalse :: SBool 'False

newtype DepClient (ix :: Bool -> *) (f :: Bool ~> Type) =
    DepClient {runDepClient :: forall (b :: Bool) . ix b -> Client (Apply f b)}

data SideLoadTerminal :: method -> status -> cts -> a -> deps -> (Bool ~> Type) where
  SideLoadTerminal :: SideLoadTerminal method status cts a deps b

type instance Apply (SideLoadTerminal method status cts a deps) b =
  If b (Verb method status cts (SideLoaded a deps)) (Verb method status cts a)

instance {-# OVERLAPPABLE #-}
         ( MimeUnrender JSON a
         , MimeUnrender JSON (SideLoaded a deps)
         , ReflectMethod method
         ) => HasClient (Verb method status cts a :> SideLoad deps) where

  type Client (Verb method status cts a :> SideLoad deps) = DepClient SBool (SideLoadTerminal method status cts a deps)
  clientWithRoute Proxy req = DepClient $ \sb ->
    case sb of
      STrue -> let req' = appendToQueryString "sideload" Nothing req
               in snd <$> performRequestCT (Proxy @JSON) method req'
      SFalse -> snd <$> performRequestCT (Proxy @JSON) method req
    where method = reflectMethod (Proxy @method)

--instance (FromJSON a, FromJSON (SideLoaded a deps)) => HasClient (DepFlag b :> (ExecSideLoad a deps)) where
--
--    type Client (DepFlag b :> (ExecSideLoad a deps)) = SBool b -> Apply (ExecSideLoad a deps) b
--
--    clientWithRoute Proxy req flag =
--      case flag of
--        STrue -> let req' = appendToQueryString "sideload" Nothing req
--                 in snd <$> performRequestCT (Proxy @JSON) method req'
--        SFalse -> snd <$> performRequestCT (Proxy @JSON) method req
--      where method = reflectMethod (Proxy @GET)

--    route Proxy (DepServer subserver) request respond =
--      case processedPathInfo request of
--        (p:ps) -> wd
--          case fromText p :: Maybe (Some ix) of
--            Nothing ->
--              respond $ failWith NotFound
--            Just (Some (p' :: ix a)) ->
--              case hasDepServer (Proxy :: Proxy f) p' of
--                Dict -> route (Proxy :: Proxy (Apply f a))
--                              (subserver p')
--                              request{ pathInfo = ps }
--                              respond
--        _ ->
--          respond $ failWith NotFound



--instance {-# OVERLAPPABLE #-}
--  (MimeUnrender ct (SideLoaded a deps), ReflectMethod method, cts' ~ (ct ': cts)
--  ) => HasClient (Verb method status cts' a :> SideLoad deps) where
--  type Client (Verb method status cts' a :> SideLoad deps) = Sing Bool -> ClientM (SideLoaded a deps)
--  clientWithRoute Proxy req flag =
--    let req' = if flag then appendToQueryString "sideload" Nothing req else req
--    in snd <$> performRequestCT (Proxy :: Proxy ct) method req'
--      where method = reflectMethod (Proxy :: Proxy method)



--instance (KnownSymbol sym, HasClient api)
--      => HasClient (QueryFlag sym :> api) where
--
--  type Client (QueryFlag sym :> api) =
--    Bool -> Client api
--
--  clientWithRoute Proxy req flag =
--    clientWithRoute (Proxy :: Proxy api)
--                    (if flag
--                       then appendToQueryString paramname Nothing req
--                       else req
--                    )
--
--    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
