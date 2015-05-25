{-# LANGUAGE PackageImports, TypeSynonymInstances, FlexibleInstances #-}
module Prelude(
    module CorePrelude,
    module IORef,
    (++),
    unpack,
    error,
    P.show
) where
import CorePrelude hiding (on, error)
import qualified "base" Prelude as P
import qualified Data.Text as T
import Data.IORef as IORef


class Concatable a where
    (++) :: a -> a -> a

instance Concatable [a] where
    (++) = (P.++)

instance Concatable T.Text where
    (++) = T.append

class StringLike a where
    unpack :: a -> String


instance StringLike String where
    unpack = id

instance StringLike T.Text where
    unpack = T.unpack

error :: StringLike string => string -> b 
error = P.error . unpack