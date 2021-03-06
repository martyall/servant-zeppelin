{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Zeppelin.Swagger
  (
  -- | The purpose of this package is provide the instance for 'servant-zepplin'
  -- combinators needed for 'servant-swagger' documentation generation.
    SideLoad
  ) where

import           Control.Lens               (mapped, (%~), (&), (.~), (?~))
import           Control.Monad
import           Data.Aeson                 (ToJSON (..))

import qualified Data.HashMap.Strict.InsOrd as O (empty, fromList, insert,
                                                  member)
import           Data.Kind
import           Data.Monoid                ((<>))
import           Data.Promotion.Prelude     hiding ((:>))
import           Data.Singletons.TypeLits
import           Data.Swagger
import           Data.Swagger.Declare
import           Data.Text                  as T
import           Servant.API
import           Servant.Swagger.Internal

import           Servant.Zeppelin
import           Servant.Zeppelin.Internal.Types

--------------------------------------------------------------------------------

-- | Helper type class for collecting the 'NamedSchema's of the dependencies.
class ToDependencySchema (deps :: [*]) where
  declareDependencySchema :: proxy deps -> Declare (Definitions Schema) NamedSchema

-- | Base case for induction.
instance ToDependencySchema '[] where
  declareDependencySchema _ =
    return $ NamedSchema Nothing
      ( mempty
        & type_ .~ SwaggerObject
        & properties .~ O.empty
      )

-- | Inductive step.
instance ( ToDependencySchema deps
         , KnownSymbol (NamedDependency d)
         , ToSchema d
         ) => ToDependencySchema (d : deps) where
  declareDependencySchema _ = do
    dRef <- declareSchemaRef $ Proxy @d
    let dText = T.pack . symbolVal $ Proxy @(NamedDependency d)
    declareDependencySchema (Proxy  :: Proxy deps)
      & mapped.schema.properties %~ O.insert dText dRef

instance ( ToSchema a
         , ToDependencySchema deps
         ) => ToSchema (SideLoaded a deps) where
  declareNamedSchema _ = do
    aRef <- declareSchemaRef $ Proxy @a
    depsRef <- declareDependencySchemaRef $ Proxy @deps
    let aName = schemaName $ Proxy @a
    return $ NamedSchema (fmap ("side-loaded JSON: " <>) aName)
      ( mempty
        & type_ .~ SwaggerObject
        & properties .~ O.fromList [("data", aRef), ("dependencies", depsRef)]
      )

--  | PolyKinded version of declareSchemaRef.
declareDependencySchemaRef :: ToDependencySchema deps
                           => proxy deps
                           -> Declare (Definitions Schema) (Referenced Schema)
declareDependencySchemaRef deps =
  case undeclare . declareDependencySchema $ deps of
    NamedSchema (Just schmName) schm -> do
      known <- looks (O.member schmName)
      unless known $ do
        declare $ O.fromList [(schmName, schm)]
        void $ declareDependencySchema deps
      return $ Ref (Reference schmName)
    _ -> Inline . _namedSchemaSchema <$> declareDependencySchema deps

instance {-# OVERLAPPABLE #-}
         ( ToSchema a
         , ToDependencySchema deps
         , AllAccept cs
         , KnownNat status
         , SwaggerMethod method
         )
  => HasSwagger (Verb method status cs a :> SideLoad deps) where
  toSwagger _ =
    toSwagger (Proxy @(Verb method status cs (SideLoaded a deps)))
      & addParam param
      where
      param = mempty
        & name .~ "sideload"
        & schema .~ ParamOther (mempty
            & in_ .~ ParamQuery
            & allowEmptyValue ?~ True
            & paramSchema .~ (toParamSchema (Proxy :: Proxy Bool)
                & default_ ?~ toJSON False))
