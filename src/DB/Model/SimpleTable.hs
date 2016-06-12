module DB.Model.SimpleTable 
   (SimpleTable(..), 
    Relation(..),
    module X) where

import DB.Model.MultiTable as X hiding (Relation(..), relation)
import DB.Model.Internal.SimpleTable
