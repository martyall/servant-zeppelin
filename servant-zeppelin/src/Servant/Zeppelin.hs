module Servant.Zeppelin where

import           Data.Kind

-- | Combinator to indicate the availablity of side loaded data.
--
-- > "albums" :> Get '[JSON] Album :> SideLoad '[Person, [Photo]]
data SideLoad (fs :: [*])
