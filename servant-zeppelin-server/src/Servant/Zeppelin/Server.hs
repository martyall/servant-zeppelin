module Servant.Zeppelin.Server

  (
  -- * Necessary for using 'SideLoad' @deps@.
    Inflatable(..)
  , DependencyList(..)
  , HasDependencies(..)
  -- * Type family for JSON object keys.
  , NamedDependency
  -- * Servant combinator
  , SideLoad

  ) where

import           Servant.Zeppelin.Server.Internal              ()
import           Servant.Zeppelin.Server.Internal.ContentTypes ()
import           Servant.Zeppelin.Internal.Types
import           Servant.Zeppelin
