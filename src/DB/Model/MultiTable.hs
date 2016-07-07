{-|
Module      : DB.Monad.MultiTable
Description : An interface designed to put data from multiple tables into a data
Copyright   : (c) Yu Li
License     : MIT
Maintainer  : ylilarry@gmail.com
Stability   : experimental
Portability : GHC extensions

This is an interface designed to put data from multiple tables into a Haskell data.

The module uses "Database.HDBC" as the backend, so it supports whatever "Database.HDBC" supports.

If you are learning to use this library, this is the best place to start.

== Show case

@
   
   data User m = User {
      key   :: m Integer,
      name  :: m String,
      email :: m String
   } deriving (Generic)
   
   instance MultiTable User where
      relation = User {
         key   = IsKey [("user_table", "user_id")],
         name  = IsCol "user_table" "user_name",
         email = IsCol "user_table" "user_email"
      }
      
   showCase :: Model ()
   showCase = do
    
      __ ex <- load (name =. "bob") __
      -- / prepares SELECT user_id, user_name, user_email FROM user_table WHERE user_name=? /
      -- / execute the statement with user_name="bob" /
      
      __ print (ex :: User Value) __
      >>> User { key = Val 3, name = Val "bob", email = Val "bob\@example.com" }
      
      __ save ex __
      -- / prepares INSERT INTO user_table (user_name, user_email) VALUES (?, ?) /
      -- / execute the statement with /
      -- / user_name="YU LI", /
      -- / user_email="bob\@example.com" /
      -- / fetch last_insert_id(); /
      
      __ print $ ex # key __
      >>> 219  -- new row inserted with user_id=219

      __ update (ex {email = Val "ylilarry\@gmail.com"}) __
      -- / prepares UPDATE user_table SET user_name=?, user_email=? WHERE user_id=? /
      -- / execute the statement with /
      -- / user_name="YU LI", /
      -- / user_email="ylilarry\@example.com", /
      -- / user_id=219 /

      __ remove ex __
      -- / prepares DELETE FROM user_table WHERE user_id=? /
      -- / execute the statement with /
      -- / user_id=219 /
      
@

Data that are instances of 'MultiTable' must satisfy:

   * Data must be @ ((* -> *) -> *) @
   * Each record field of the data must be @ (* -> *) @
   * Data must derive 'Generic'
   * Data must be an instance of 'MultiTable'
   * Data must have exactly one field for a unique auto-increasing primary key (integer) in the database that identifies the data

-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DB.Model.MultiTable 
      (-- | = Basic database
       Table,
       Column,
       Relation(..),
       MultiTable(..), 
       Value(..),
       Where,
       WhereBuilder(..),
       -- | = Model monad
       Model(..),
       ModelT(..),
       runModelT,
       mapModelT,
       -- | = Utility functions
       rawQuery,
       cast,
       (?<),
       (#),
       -- | = Re-export
       Generic) 
   where
   
import DB.Model.Internal.MultiTable hiding (relation')
import DB.Model.Internal.Value
import DB.Model.Internal.Where
import DB.Model.Internal.Class
import DB.Model.Internal.Prelude
