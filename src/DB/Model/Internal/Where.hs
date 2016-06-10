module DB.Model.Internal.Where where

import DB.Model.Internal.Prelude
import DB.Model.Internal.Class

import           Data.Aeson ()
import qualified Data.Aeson as A


data CondExp = Compare Table String A.Value
             | Func String [CondExp]
            deriving             

type WhereBool t = Reader (t Relation) CondExp

class WhereVal a where
   lt :: (t Relation -> Relation a) -> a -> WhereBool t
   lt selector v = do
      rel <- selector <$> ask
      return $ Compare (getTable rel) "<" (unsafeTo v)
            
    
   le :: (t Relation -> Relation a) -> a -> WhereBool t
   
   gt :: (t Relation -> Relation a) -> a -> WhereBool t
   ge :: (t Relation -> Relation a) -> a -> WhereBool t
   
   eq :: (t Relation -> Relation a) -> a -> WhereBool t
   
   and :: WhereBool t -> WhereBool t -> WhereBool t
   
getTable :: Relation a -> String
getTable (IsCol t c) = t
getTable (IsKey ls) = fst $ head ls
