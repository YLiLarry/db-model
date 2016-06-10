{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module DB.Model.MultiTable 
   (ModelT, Model, Value(..), MultiTable(..), Relation(..), runModelT) where

import DB.Model.Internal.Prelude
import DB.Model.Internal.TypeCast
import           DB.Model.Internal (Relation(..), Where, Model, ModelT, Value(..), TableBase, Table, Column, runModelT)
import qualified DB.Model.Internal as I
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Aeson ()
import qualified Data.Aeson as A
import           Data.List ()
import qualified Data.List as L

-- instance (Generic (a Relation), GFromJSON (Rep (a Relation))) => FromJSON (a Relation)
-- instance (Generic (a Relation), GToJSON (Rep (a Relation))) => ToJSON (a Relation)

instance (MultiTable a, Generic (a m), GToJSON (Rep (a m))) => ToJSON (a m)
instance (MultiTable a, Generic (a m), GFromJSON (Rep (a m))) => FromJSON (a m)
instance (MultiTable a, Generic (a m), GShow' (Rep (a m))) => GShow (a m)

instance {-# OVERLAPPABLE #-} (GShow a) => Show a where
   show = gshow
   
class (Typeable a,
       FromJSON (a Relation),
       ToJSON (a Relation), 
       ToJSON (a Value),
       FromJSON (a Value),
       Show (a Relation),
       Show (a Value)) => MultiTable (a :: (* -> *) -> *) where
   
   -- ^ a map from fields to table and column names
   relation :: a Relation
   
   -- relation' :: Proxy a -> [(String, Relation A.Value)]
   -- relation' = const $ M.toList $ unsafeTo $ getRelation (relation :: a Relation)
   
   -- keyName :: (MonadError String m) => Proxy a -> m (String, String)
   -- keyName = const $ do
   --    case L.find isKey $ relation' (Proxy :: Proxy a) of
   --       Nothing -> throwError $ printf "Cannot find an IsKey field in %s" (show (relation :: a Relation))
   --       Just (field, IsKey _ col) -> return (field, col)
   --    where 
   --       isKey :: (String, Relation A.Value) -> Bool
   --       isKey (_, IsKey _ _) = True
   --       isKey _ = False
      
   -- ^ a function that gives the value of the primary key
   getKey :: a x -> Integer
   
   load :: (IConnection con) => Where -> Model con [a Value]
   load = loadR (relation :: a Relation)
   
   loadR :: (IConnection con) => a Relation -> Where -> Model con [a Value]
   loadR r w = map (unsafeTo . kvp2json) <$> (I.recursive (Proxy :: Proxy I.Load) (obj2kvp r) w undefined)
   
   new :: (IConnection con) => a Value -> Model con (a Value)
   new = newR (relation :: a Relation)
   
   newR :: (IConnection con) => a Relation -> a Value -> Model con (a Value)
   
   update :: (IConnection con) => a Value -> Model con ()
   update = updateR (relation :: a Relation)
   
   updateR :: (IConnection con) => a Relation -> a Value -> Model con ()
   updateR r v = void $ (I.recursive (Proxy :: Proxy I.Update) (obj2kvp r) undefined (obj2kvp v))
   
   remove :: (IConnection con) => a Value -> Model con ()
   remove = removeR (relation :: a Relation)
   
   removeR :: (IConnection con) => a Relation -> a Value -> Model con ()
   
-- deriving instance (MultiTable a) => Generic (a Relation)

-- instance (MultiTable a) => TableBase a

-- instance (MultiTable a, GShow (a Relation)) => Show (a Relation)

