module DB.Model.Internal.Update where

import DB.Model.Internal.Prelude
import DB.Model.Internal.Class
import DB.Model.Internal.TypeCast
import DB.Model.Internal.Value

import qualified Data.Aeson as A
import qualified Data.List as L

data Update = Update String [A.Value] deriving (Show)

recursiveUpdate :: (IConnection con) => [(String, Relation A.Value)] -> [(String, A.Value)] -> Model con [[(String, Value A.Value)]]
recursiveUpdate r v = do
   let nulls  = map (const Null `second`) $ filter (isNull . snd) r
   let consts  = map (getConst `second`) $ filter (isConst . snd) r
   let hasMany = filter (isHasMany . snd) r
   let others  = filter (isColOrKey . snd) r
   result <- retrieve (group others v)
   let ids = [ findKeyVal r o | o <- result ]
   recurs <- sequence [ sequence [ sequence (field, Many <$> map kvp2json <$> (handleSubObj field (unsafeTo id) r v)) | (field, r) <- hasMany] | id <- ids ]
   return $ zipWith (++) recurs [ nulls ++ map (wrapValue `second`) (consts ++ object) | object <- result ]
   where
      wrapValue :: A.Value -> Value A.Value
      wrapValue A.Null = Null
      wrapValue x = Val x
      
      retrieve :: (IConnection con) => [([String], Update)] -> Model con [[(String, A.Value)]]
      retrieve a = regroup <$> mapM (sndM execQuery) a
               
      handleSubObj :: (IConnection con) => String -> Integer -> Relation A.Value -> [(String, A.Value)] -> Model con [[(String, Value A.Value)]]
      handleSubObj field id (HasMany idCol r) v = recursiveUpdate (json2kvp r) val
         where val = if null v then [] else json2kvp $ fromJust $ L.lookup field v

      group :: [(String, Relation A.Value)] -> [(String, A.Value)] -> [([String], Update)]
      group rel vals = [ ([], Update (printf "UPDATE %s SET %s WHERE %s=%d" 
                                             table (setCaluse table) keyCol keyVal)
                                       (findVals table))
                           | (table, keyCol) <- keys ]
         where
            (IsKey keys) = snd $ findIsKey rel 
            keyVal = findKeyVal rel vals
            setCaluse t = L.intercalate "," [ printf "%s=?" c | f <- fields t, let (IsCol _ c) = relForField f rel ]
            fields t = mapMaybe (equalsTable t) rel
            -- find all non-key cols that is in table t
            equalsTable t (f, IsCol t1 _) | t == t1 = Just f 
            equalsTable _ _ = Nothing
            findVals t = [ val2aeson $ unsafeTo $ fromJust $ L.lookup f vals | f <- fields t ]
            val2aeson :: Value A.Value -> A.Value
            val2aeson (Val x) = toJSON x
            val2aeson Null = A.Null

      execQuery :: (IConnection con) => Update -> Model con [[A.Value]] 
      execQuery (Update q vals) = do
         cnn <- ask
         liftIO $ (map.map) sql2aeson <$> withTransaction cnn (\cnn -> quickQuery cnn q (map aeson2sql vals))
