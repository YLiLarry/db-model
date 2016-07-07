{-|
Module      : DB.Model.SimpleTable
Description : A version that assumes a one-to-one relation between Haskell data and the table.
Copyright   : (c) Yu Li
License     : MIT
Maintainer  : ylilarry@gmail.com
Stability   : experimental
Portability : GHC extensions

This is a version of interface that assumes a one-to-one relation between Haskell data and the table.

__The functionality of this module is a strict subset of "DB.Model.MultiTable".__

== Show case

@
   data User m = User {
      key   :: m Integer,
      name  :: m String,
      email :: m String
   } deriving (Generic)
   
   instance SimpleTable User where
      relation = User {
         key   = IsKey "user_table" "user_primary_id",    -- use the "user_table" table and "user_table.user_primary_id" as the unique key
         name  = IsCol "user_phone",                      -- use the @user_table.user_phone@ column
         email = IsCol "user_email"                       -- use the @user_table.user_email@ column
      }        
@
      
-}

module DB.Model.SimpleTable 
   (SimpleTable(..), 
    Relation(..),
    module X) where

import DB.Model.MultiTable as X hiding (Relation(..), relation)
import DB.Model.Internal.SimpleTable
