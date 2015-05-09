{-# LANGUAGE PackageImports #-}
module Prelude(
    module CorePrelude
) where
import CorePrelude hiding (on)
import qualified "base" Prelude
