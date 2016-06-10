module DB.Model.Internal.New where
   
import DB.Model.Internal.Prelude
import DB.Model.Internal.Class
import DB.Model.Internal.TypeCast
import DB.Model.Internal.Value

import           Data.Aeson ()
import qualified Data.Aeson as A
import           Data.List ()
import qualified Data.List as L


data New = New String [A.Value] deriving (Show)

instance Query New where
   group rel _ vals = [ ([], New (printf "INSERT INTO %s (%s) VALUES (%s)" 
                                          table 
                                          (intercalate "," cols)
                                          (intercalate "," $ map (const "?") cols))
                                 (findVals table))
                        | (table, _) <- keys, let cols = findCols table ]
      where
         (IsKey keys) = snd $ findIsKey rel 
         (Val keyVal) = unsafeTo $ findKeyVal rel vals :: Value Integer
         findCols t = mapMaybe (getCol <$> snd <$> inTable t) rel
         getCol (IsCol _ c) = c
         fields t = mapMaybe (fst <$> inTable t) rel
         -- find all non-keys that is in table t
         inTable t (f, x@(IsCol t1 _)) | t == t1 = Just x
         inTable _ _ = Nothing
         findVals t = [ val2aeson $ unsafeTo $ fromJust $ L.lookup f vals | f <- fields t ]
         val2aeson :: Value A.Value -> A.Value
         val2aeson (Val x) = toJSON x
         
   execQuery (New q vals) = do
      cnn <- ask
      liftIO $ (map.map) sql2aeson <$> withTransaction cnn (\cnn -> quickQuery cnn q (map aeson2sql vals))

