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
   (ModelT, 
    Model, 
    Value(..), 
    MultiTable(..), 
    MultiTableR(..), 
    Relation(..), 
    runModelT,
    rawQuery,
    cast,
    (#)) 
   where

import DB.Model.Internal.Prelude
import DB.Model.Internal.TypeCast
import DB.Model.Internal as I
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Aeson ()
import qualified Data.Aeson as A
import           Data.List ()
import qualified Data.List as L

-- instance (Generic (a Relation), GFromJSON (Rep (a Relation))) => FromJSON (a Relation)
-- instance (Generic (a Relation), GToJSON (Rep (a Relation))) => ToJSON (a Relation)

instance (MultiTableR a, Generic (a m), GToJSON (Rep (a m))) => ToJSON (a m)
instance (MultiTableR a, Generic (a m), GFromJSON (Rep (a m))) => FromJSON (a m)
instance (MultiTableR a, Generic (a m), GShow' (Rep (a m))) => GShow (a m)

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
   
   load :: (IConnection con) => Where -> Model con [a Value]
   load = loadR (relation :: a Relation)
   
   new :: (IConnection con) => a Value -> Model con (a Value)
   new = newR (relation :: a Relation)
   
   update :: (IConnection con) => a Value -> Model con ()
   update = updateR (relation :: a Relation)
   
   remove :: (IConnection con) => a Value -> Model con ()
   remove = removeR (relation :: a Relation)

instance {-# OVERLAPPABLE #-} (MultiTable a) => MultiTableR a 

class (Typeable a,
       FromJSON (a Relation),
       ToJSON (a Relation), 
       ToJSON (a Value),
       FromJSON (a Value),
       Show (a Relation),
       Show (a Value)) => MultiTableR (a :: (* -> *) -> *) where
   
   loadR :: (IConnection con) => a Relation -> Where -> Model con [a Value]
   loadR r w = map (unsafeTo . kvp2json) <$> (I.recursiveLoad (obj2kvp r) w)
   
   updateR :: (IConnection con) => a Relation -> a Value -> Model con ()
   updateR r v = void $ (I.recursiveUpdate (obj2kvp r) (obj2kvp v))
   
   newR :: (IConnection con) => a Relation -> a Value -> Model con (a Value)
   newR r v = (unsafeTo . kvp2json) <$> (I.recursiveNew (obj2kvp r) Nothing (obj2kvp v))
   
   removeR :: (IConnection con) => a Relation -> a Value -> Model con ()
   removeR r v = I.nonRecursiveRemove (obj2kvp r) (obj2kvp v)

   
rawQuery :: (IConnection con, 
             Typeable a, 
             FromJSON a) => Query -> [SqlValue] -> Model con [[a]]
rawQuery q vs = do
   con <- ask
   r <- liftIO $ withTransaction con (\cnn -> quickQuery cnn q vs)
   return $ (map . map) cast r

cast :: (Typeable a, FromJSON a) => SqlValue -> a
cast = unsafeTo . sql2aeson
