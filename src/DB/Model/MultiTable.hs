{-|
Module      : DB.Monad.MultiTable
Description : An interface designed to put data from multiple tables into a data
Copyright   : (c) Yu Li
License     : MIT
Maintainer  : ylilarry@gmail.com
Stability   : experimental
Portability : GHC extensions

This is an interface designed to put data from multiple tables into a Haskell data.

The module uses "Database.HDBC" as the backend, so it support whatever "Database.HDBC" supports.

If you are learning to use this library, this is the best place to start.

Show case:

@
   
   data Example m = Example {
      key    :: m Integer,
      field1 :: m Int,
      field2 :: m String
   } deriving (Generic)
   
   instance MultiTable Example where
      relation = Example {
         key   = IsKey [("table_name", "prime_key_col")],
         field1 = IsCol "column1",
         field2 = IsCol "column2"
      }
      
   main :: IO ()
   main = do
      ex <- load (key =. 3) 
      print (ex :: Example Value)
      
      -- calls SELECT prime_key_col, column1, column2 FROM table_name WHERE prime_key_col=3
      -- puts result in Example data and prints

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
      (Table,
       Column,
       Relation(..),
       MultiTable(..), 
       Value(..),
       Where,
       WhereBuilder(..),
       Model(..),
       ModelT(..),
       runModelT,
       mapModelT,
       rawQuery,
       cast,
       (?<),
       (#),
       Generic) 
   where
   
import DB.Model.Internal.MultiTable hiding (relation')
import DB.Model.Internal.Value
import DB.Model.Internal.Where
import DB.Model.Internal.Class
import DB.Model.Internal.Prelude
