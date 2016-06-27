module DB.Model.MultiTable 
      (MultiTable(..), 
       MultiTableR(..),
       Relation(..),
       Value(..),
       WhereResult(..),
       Table,
       Column,
       (?<),
       (#),
       cast,
       Model(..),
       ModelT(..),
       rawQuery,
       runModelT,
       Generic(..)) 
   where
   
import DB.Model.Internal.MultiTable
import DB.Model.Internal.Value
import DB.Model.Internal.Where
import DB.Model.Internal.Class
import DB.Model.Internal.Prelude
