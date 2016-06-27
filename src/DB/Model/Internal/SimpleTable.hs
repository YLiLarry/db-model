{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module DB.Model.Internal.SimpleTable where

import DB.Model.Internal.Prelude
import DB.Model.Internal.Value
import DB.Model.MultiTable (MultiTable)
import           DB.Model.Internal.Class (Table, Column)
import qualified DB.Model.Internal.Class as M
import qualified DB.Model.MultiTable as M

import qualified Data.List as L
import qualified Data.Aeson as A


data Relation t = IsCol Column -- ^ Column name in the table used for this object
                | IsKey Table Column -- ^ Table name, column name. Current field is a key. 
                | HasMany Column t
                | IsConst t
                | IsNull
               deriving (Generic)
               
instance (FromJSON x) => FromJSON (Relation x)
instance (ToJSON x) => ToJSON (Relation x)
instance (GShow x) => GShow (Relation x)
instance (GEq x) => GEq (Relation x)

class (Typeable a, 
       FromJSON (a Relation),
       ToJSON (a Relation),
       Show (a Relation),
       FromJSON (a Value),
       ToJSON (a Value),
       Show (a Value),
       FromJSON (a M.Relation),
       ToJSON (a M.Relation),
       Show (a M.Relation)) => SimpleTable (a :: (* -> *) -> *) where
   relation :: a Relation
   
instance (SimpleTable a) => MultiTable a where
   relation = kvp2obj newRel
      where
         (keyField, IsKey table keyCol) = fromJust $ L.find (isKey . snd) kvp
         
         kvp :: [(String, Relation A.Value)]
         kvp = obj2kvp (relation :: a Relation)
         
         addTable :: Relation A.Value -> M.Relation A.Value
         addTable (IsCol c) = M.IsCol table c
         addTable (IsKey t c) = M.IsKey [(t,c)]
         addTable (HasMany c v) = M.HasMany c v
         addTable (IsConst v) = M.IsConst v
         addTable IsNull = M.IsNull
         
         newRel :: [(String, M.Relation A.Value)]
         newRel = map (second addTable) kvp
         
isKey :: Relation t -> Bool
isKey (IsKey _ _) = True
isKey _ = False
