{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}

module DB.Model.Internal.MultiTable where

import DB.Model.Internal.Prelude as I
import DB.Model.Internal.TypeCast as I
import DB.Model.Internal.Class as I
import DB.Model.Internal.Load as I
import DB.Model.Internal.New as I
import DB.Model.Internal.Remove as I
import DB.Model.Internal.Update as I
import DB.Model.Internal.Value as I
import DB.Model.Internal.Where as I

import qualified Data.Aeson as A
import qualified Data.List as L

instance (MultiTable a, Generic (a m), GToJSON (Rep (a m))) => ToJSON (a m)
instance (MultiTable a, Generic (a m), GFromJSON (Rep (a m))) => FromJSON (a m)
instance (MultiTable a, Generic (a m), GShow' (Rep (a m))) => GShow (a m)
instance (MultiTable a, Generic (a m), GEq' (Rep (a m))) => GEq (a m)

instance {-# OVERLAPPABLE #-} (GShow a) => Show a where
   show = gshow
instance {-# OVERLAPPABLE #-} (GEq a) => Eq a where
   (==) = geq


-- | 'MultiTable' has long subclasses constraints but fear not! 
--
--   __ For all these instances are automatically derived as long as your data derives 'Generic'. __
class (Typeable a,
       FromJSON (a Relation),
       ToJSON (a Relation), 
       FromJSON (a Value),
       ToJSON (a Value),
       Show (a Relation),
       Show (a Value)) => MultiTable (a :: (* -> *) -> *) where
   
   -- | Define the default relation, a mapping from fields to database's tables and columns.
   --
   -- Example:
   --
   -- @
   --    instance MultiTable Example where
   --       relation = Example {
   --          key   = IsKey [("table_1", "prime_key_col"), ("table_2", "prime_key_col")],
   --          field1 = IsCol "table_1" "column_name" -- field uses data in table_1.column_name where prime_key_col=?
   --          field2 = IsCol "table_2" "column_name" -- field uses data in table_2.column_name where prime_key_col=?
   --       }
   -- @
   --
   --   There must be exactly one field labelled 'IsKey', otherwise a 'UserError' will be thrown.
   relation :: a Relation
   
   relation' :: a Relation
   relation' 
      | isNothing (L.find (isKey . snd) kvp) = error $ printf "No IsKey field for %s." $ show rel
      | otherwise = rel
      where 
         rel :: a Relation
         rel = relation
         kvp :: [(String, Relation A.Value)]
         kvp = obj2kvp rel 
   
   -- | Load data from the database, according to the default relation.
   load :: (IConnection con) => Where a -> Model con [a Value]
   load = loadR relation'
   
   -- | Create value in the database, according to the default relation.
   new :: (IConnection con) => a Value -> Model con (a Value)
   new = newR relation'
   
   -- | Update the value in the database, according to the default relation.
   update :: (IConnection con) => a Value -> Model con ()
   update = updateR relation'
   
   -- | Delete the value from the database, according to the default relation.
   remove :: (IConnection con) => a Value -> Model con ()
   remove = removeR relation'
   
   -- | Same as 'load' except it uses the first argument as the ralation instead of the default one.
   loadR :: (IConnection con) => a Relation -> Where a -> Model con [a Value]
   loadR r w = map (unsafeTo . kvp2json) <$> (I.recursiveLoad (obj2kvp r) wc)
      where wc = runReader w r
   
   -- | Same as 'update' except it uses the first argument as the ralation instead of the default one.
   updateR :: (IConnection con) => a Relation -> a Value -> Model con ()
   updateR r v = void $ (I.recursiveUpdate (obj2kvp r) (obj2kvp v))
   
   -- | Same as 'new' except it uses the first argument as the ralation instead of the default one.
   newR :: (IConnection con) => a Relation -> a Value -> Model con (a Value)
   newR r v = (unsafeTo . kvp2json) <$> (I.recursiveNew (obj2kvp r) Nothing (obj2kvp v))
   
   -- | Same as 'remove' except it uses the first argument as the ralation instead of the default one.
   removeR :: (IConnection con) => a Relation -> a Value -> Model con ()
   removeR r v = I.nonRecursiveRemove (obj2kvp r) (obj2kvp v)

-- | Allows you to run a raw SQL query. 
--   This is a wrapper for 'quickQuery' in HDBC
rawQuery :: (IConnection con, 
             Typeable a, 
             FromJSON a) => Query -> [SqlValue] -> Model con [[a]]
rawQuery q vs = do
   con <- ask
   r <- liftIO $ withTransaction con (\cnn -> quickQuery cnn q vs)
   return $ (map . map) (cast . sql2aeson) r

-- | Helper function:
--
-- @
--    [[a,b,c]] <- rawQuery ...
--    printf "%d" $ (cast a :: Integer)
-- @
--
cast :: (Typeable a, FromJSON a) => JSONValue -> a
cast = unsafeTo
