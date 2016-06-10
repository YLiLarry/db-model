{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module DB.Model.Interface where

import           Database.HDBC (IConnection)
import qualified Database.HDBC as H
import           Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as R
import           DB.Model.Internal (Value, Key, Load, Save)
import qualified DB.Model.Internal as I


class ModelInterface a where
   data Savable a
   data Loadable a
   data Removable a
   data Updatable a
   save :: Savable a
   load :: Loadable a
   remove :: Removable a
   update :: Updatable a
   -- save :: Signature a (IConnection con) => a Value -> ReaderT con IO [a Key]
   -- load :: (IConnection con) => a Load -> ReaderT con IO [a Value]
   -- remove :: (IConnection con) => a Value -> ReaderT con IO ()
   -- update :: (IConnection con) => a Value -> ReaderT con IO ()
   
instance ModelInterface Int where
   data Savable a = TEST
instance ModelInterface Integer where
   data Savable a = TEST
