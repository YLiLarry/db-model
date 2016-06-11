{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DB.Model.Internal.Value where

import GHC.Generics hiding (to, from)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Database.HDBC as D hiding (run)
import Text.Printf
import           Data.Aeson (fromJSON, toJSON, FromJSON, ToJSON, GToJSON, GFromJSON)
import qualified Data.Aeson as A
import Control.Arrow
import Data.List as L
import Data.List.Split as L
import           Data.Map (Map)
import qualified Data.Map as M
import DB.Model.Internal.Exception
import DB.Model.Internal.TypeCast
import qualified Data.Vector as V
import Data.Maybe
import Control.Exception
import GHC.IO.Exception
import Generics.Deriving.Show
import Data.Typeable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import           Data.Proxy (Proxy(..))
import qualified Data.Proxy as P

data Value x = Val x
             | Many [x]
             | Null
         deriving (Generic, Show, Eq, Functor)

instance (ToJSON x) => ToJSON (Value x)
instance (FromJSON x) => FromJSON (Value x)
instance (GShow x) => GShow (Value x)

get :: (a Value -> Value b) -> a Value -> b
get f a = let Val x = f a in x

(#) = flip get

-- instance Applicative Value where
--    pure = Val
--    (Val f) <*> (Val a) = (Val $ f a)
--    (Val f) <*> (Many a) = (Many $ map f a)
--    _ <*> _ = Null


-- instance WrapVal Value where
--    wrapVal = Val 
--    wrapVals = Many
   
-- instance Field Value
