module DB.MonadModel.SimpleTable 
   (SimpleTable(..), 
    Relation(..),
    module X) 
   where
   
import DB.MonadModel.MultiTable as X hiding (Relation(..), relation)
import DB.Model.Internal.SimpleTable

