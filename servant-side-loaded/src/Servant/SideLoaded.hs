{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module Servant.SideLoaded where

data SideLoad (deps :: [*])
