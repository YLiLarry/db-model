module DB.Model.Internal.Update where

import DB.Model.Internal.Prelude
import DB.Model.Internal.Class
import DB.Model.Internal.TypeCast
import DB.Model.Internal.Value

import           Data.Aeson ()
import qualified Data.Aeson as A
import           Data.List ()
import qualified Data.List as L


data Update = Update String [A.Value] deriving (Show)

instance Query Update where
   group rel _ vals = [ ([], Update (printf "UPDATE %s SET %s WHERE %s=%d" 
                                          table (setCaluse table) keyCol keyVal)
                                    (findVals table))
                        | (table, keyCol) <- keys ]
      where
         (IsKey keys) = snd $ findIsKey rel 
         (Val keyVal) = unsafeTo $ findKeyVal rel vals :: Value Integer
         setCaluse t = L.intercalate "," [ printf "%s=?" c | f <- fields t, let (IsCol _ c) = findForField f rel ]
         fields t = mapMaybe (equalsTable t) rel
         -- find all non-key cols that is in table t
         equalsTable t (f, IsCol t1 _) | t == t1 = Just f 
         equalsTable _ _ = Nothing
         findVals t = [ val2aeson $ unsafeTo $ fromJust $ L.lookup f vals | f <- fields t ]
         val2aeson :: Value A.Value -> A.Value
         val2aeson (Val x) = toJSON x

   execQuery (Update q vals) = do
      cnn <- ask
      liftIO $ (map.map) sql2aeson <$> withTransaction cnn (\cnn -> quickQuery cnn q (map aeson2sql vals))
