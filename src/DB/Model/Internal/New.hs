module DB.Model.Internal.New where
   
import DB.Model.Internal.Prelude
import DB.Model.Internal.Class
import DB.Model.Internal.TypeCast
import DB.Model.Internal.Value

import qualified Data.Aeson as A
import qualified Data.List as L


data New = New Table [Column] [A.Value] deriving (Show)


recursiveNew :: (IConnection con) => [(String, Relation A.Value)] -> Maybe (Column, A.Value) -> [(String, A.Value)] -> Model con [(String, A.Value)]
recursiveNew r frgn v = do
   -- primary table
   cnn <- ask
   [[id']] <- liftIO $ withTransaction cnn 
      (\cnn -> do 
         quickQuery cnn stmt (map val2sql pVals)
         quickQuery cnn "SELECT last_insert_rowid();" [])
   let id = sql2aeson id'
   -- other tables
   sequence_ [ newOther other
                        (fromJust $ L.lookup tb otherTB)
                        id
                        [ (field, fromJust $ L.lookup field v) | (field, IsCol _ c) <- other ]
               | other <- inOthers, let (IsCol tb _) = snd $ head other ] 
   -- sub elements
   sequence_ [ recursiveNew (json2kvp rel) 
                            (Just (foreignCol, id))
                            (json2kvp $ fromJust $ L.lookup field v) 
               | (field, (HasMany foreignCol rel)) <- hasMany ]
   return [ if w == primField then (w, unsafeTo $ Val id) else (w,v) | (w, v) <- v ]
   where
      (primField, (IsKey ((primTB,primCol):otherTB))) = findIsKey r
      -- primary table rel
      inPrim :: [(String, Relation A.Value)]
      inPrim = filter (inTable primTB) r
      -- other table rel
      inOthers :: [[(String, Relation A.Value)]]
      inOthers = groupSortOn (getTable . snd) $ filter isInOthers r
      isInOthers (_, IsCol t _) = t /= primTB
      isInOthers _ = False
      getTable (IsCol t _) = t
      -- recursive rel
      hasMany :: [(String, Relation A.Value)]
      hasMany = filter (isHasMany . snd) r

      colvals :: [(Column, A.Value)]
      colvals = [ (col, fromJust $ L.lookup field v) | (field, (IsCol _ col)) <- inPrim ]
                  ++ if isJust frgn then [fromJust frgn] else []
         
      pCols :: [Column]
      pCols = map fst colvals
      
      pVals :: [A.Value]
      pVals = map snd colvals
      
      stmt :: String
      stmt = printf "INSERT INTO %s (%s) VALUES (%s)" 
               primTB 
               (L.intercalate "," pCols) 
               (L.intercalate "," $ map (const "?") pCols)
      
inTable :: Table -> (String, Relation A.Value) -> Bool
inTable t (f, (IsCol t1 _)) = t == t1
inTable _ _ = False

-- IsCol only
newOther :: (IConnection con) => [(String, Relation A.Value)] -> Column -> A.Value -> [(String, A.Value)] -> Model con ()
newOther rel frgnKeyCol frgnKeyVal vals = do
   cnn <- ask
   void $ liftIO $ withTransaction cnn 
      (\cnn -> quickQuery cnn stmt (map val2sql qVals))
   where
      stmt = printf "INSERT INTO %s (%s) VALUES (%s)" 
               table 
               (L.intercalate "," qCols) 
               (L.intercalate "," $ map (const "?") qCols)
      qCols = frgnKeyCol : [ c | (_, IsCol _ c) <- rel ]
      qVals = frgnKeyVal : [ fromJust $ L.lookup f vals | (f, _) <- rel ]
      (_, IsCol table _) = head rel

val2sql :: A.Value -> SqlValue
val2sql a@(A.Number key) = aeson2sql a
val2sql v = aeson2sql $
   case a of
      (Val x) ->  x
      Null -> A.Null
   where a = unsafeTo v


