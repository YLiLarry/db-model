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
   
   -- ^ a map from fields to column names
   locate :: a Relation
   
   -- ^ set the table name
   -- ^ default = Tagged (name of the data type)
   tableName :: Tagged a String
   tableName = Tagged $ show $ typeOf (Proxy :: Proxy (a Value))
   
   locate' :: Proxy a -> HashMap String (Relation A.Value)
   locate' = const $ unsafeTo (locate :: a Relation)
   
   keyColumnName :: (MonadError String m) => Proxy a -> m String
   keyColumnName = const $ do
      case find isKey $ M.toList (locate' (Proxy :: Proxy a)) of
         Nothing -> fail $ printf "Cannot find an IsKey field in %s" (show (locate :: a Relation))
         Just (field, IsKey result) -> return result
      where 
         isKey :: (String, Relation A.Value) -> Bool
         isKey (_, IsKey _) = True
         isKey _ = False
   
   -- ^ a function that gives the value of the primary key
   getKey :: a x -> Integer
   
   -- ^ load the row by the primary key
   loadKeyEQ :: (IConnection con) => Integer -> a LoadT -> Model con (a Value)
   loadKeyEQ keyVal conf = do
      key <- keyColumnName (Proxy :: Proxy a)
      result <- load $ LoadQ conf $ printf "'%s' = %d" key key
      case result of 
         [] -> throwError $ printf "Cannot find '%s' = %d in '%s'" key keyVal table
         ls -> return $ head ls
      where 
            table = untag (tableName :: Tagged a String)

instance (Table a) => MultiTable a
