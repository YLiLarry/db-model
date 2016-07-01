{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DB.Model.MultiTable 
      (MultiTable(..), 
       Relation(..),
       Value(..),
       WhereBuilder(..),
       Where,
       Table,
       Column,
       (?<),
       (#),
       cast,
       Model(..),
       ModelT(..),
       rawQuery,
       runModelT,
       mapModelT,
       Generic(..)) 
   where
   
import DB.Model.Internal.MultiTable
import DB.Model.Internal.Value
import DB.Model.Internal.Where
import DB.Model.Internal.Class
import DB.Model.Internal.Prelude
