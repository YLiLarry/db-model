{-# LANGUAGE FlexibleInstances #-}

module DB.Model.Internal.Exception where

import Control.Exception
import Data.Typeable
import Text.Printf

data DBModelError a = DBModelError a deriving (Show, Typeable)
data Mismatch a b = Mismatch a b String deriving (Show, Typeable)

instance (Exception a) => Exception (DBModelError a) where
   displayException (DBModelError e) = "DBModelError:\n" ++ displayException e

instance Exception (Mismatch String String) where
   displayException (Mismatch a b msg) =
      printf "%s\nExpected: %s\nActual: %s\n" msg a b

mismatch :: (Show a, Show b) => a -> b -> String -> DBModelError (Mismatch String String)
mismatch a b msg = DBModelError $ Mismatch (show a) (show b) msg
