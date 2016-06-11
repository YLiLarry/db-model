{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module DB.Model.Table where

import           DB.Model.Internal (Query, Where, Model, ModelT, Value, Key, unsafeTo, TableBase, unsafeTo)
import qualified DB.Model.Internal as I
import           Control.Monad.Except (ExceptT, MonadError, throwError)
import qualified Control.Monad.Except as E
import           Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader as R
import           Text.Printf (printf)
import qualified Text.Printf as P
import           Database.HDBC (IConnection)
import qualified Database.HDBC as H
import           Data.Typeable (Typeable, typeOf)
import qualified Data.Typeable as T
import           Data.Proxy (Proxy(..))
import qualified Data.Proxy as P
import           Data.Tagged (Tagged(..), untag)
import qualified Data.Tagged as T
import           Data.Aeson (FromJSON, ToJSON, GFromJSON, GToJSON, fromJSON, toJSON)
import qualified Data.Aeson as A
import           GHC.Generics (Generic, Generic1, Rep)
import qualified GHC.Generics as G
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           DB.Model.MultiTable (MultiTable(load, loadQ), Load(..), LoadT(..))
import qualified DB.Model.MultiTable as M
import           Generics.Deriving.Show (GShow)
import qualified Generics.Deriving.Show as S
import           Data.List (find)
import qualified Data.List as L


data Relation t = IsCol Column -- ^ Column name, key column name in the table used for this object
                | IsKey Column -- ^ Column name. Current field is a key. 
                | HasMany t Column
                | NoRelation
               deriving (Generic)
                       
instance (FromJSON a) => FromJSON (Relation a)
instance (ToJSON a) => ToJSON (Relation a)

class (Typeable a, 
       FromJSON (a Relation),
       ToJSON (a Relation),
       Show (a Relation)) => Table (a :: (* -> *) -> *) where
   
   
   -- ^ a map from fields to table and column names
   relation :: a Relation
   
   load :: (IConnection con) => Where -> Model con [a Value]
   load = loadR (relation :: a Relation)
   
   loadR :: (IConnection con) => a Relation -> Where -> Model con [a Value]
   loadR r w = map (unsafeTo . kvp2json) <$> (I.recursiveLoad (obj2kvp r) w)
   
   new :: (IConnection con) => a Value -> Model con (a Value)
   new = newR (relation :: a Relation)
   
   newR :: (IConnection con) => a Relation -> a Value -> Model con (a Value)
   newR r v = (unsafeTo . kvp2json) <$> head <$> (I.recursiveNew (obj2kvp r) Nothing (obj2kvp v))
   
   update :: (IConnection con) => a Value -> Model con ()
   update = updateR (relation :: a Relation)
   
   updateR :: (IConnection con) => a Relation -> a Value -> Model con ()
   updateR r v = void $ (I.recursiveUpdate (obj2kvp r) (obj2kvp v))
   
   remove :: (IConnection con) => a Value -> Model con ()
   remove = removeR (relation :: a Relation)
   
   removeR :: (IConnection con) => a Relation -> a Value -> Model con ()
