{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DB.Model.Internal.Load where

import DB.Model.Internal.Class
import DB.Model.Internal.Prelude
import DB.Model.Internal.Value
import DB.Model.Internal.Where
import DB.Model.Internal.TypeCast

import qualified Data.Aeson as A
import Data.List as L

data Load = Load String [A.Value] deriving (Show)

recursiveLoad :: (IConnection con) => [(String, Relation A.Value)] -> WhereCompiled -> Model con [[(String, Value A.Value)]]
recursiveLoad r w = do
   let nulls  = map (const Null `second`) $ filter (isNull . snd) r
   let consts  = map (getConst `second`) $ filter (isConst . snd) r
   let hasMany = filter (isHasMany . snd) r
   let others  = filter (isColOrKey . snd) r
   result <- retrieve (group others w)
   let ids = [ findKeyVal r o | o <- result ]
   recurs <- sequence [ sequence [ sequence (field, Many <$> map kvp2json <$> (handleSubObj field (unsafeTo id) r)) | (field, r) <- hasMany] | id <- ids ]
   return $ zipWith (++) recurs [ nulls ++ map (wrapValue `second`) (consts ++ object) | object <- result ]
   where
      wrapValue :: A.Value -> Value A.Value
      wrapValue A.Null = Null
      wrapValue x = Val x
      
      retrieve :: (IConnection con) => [([String], Load)] -> Model con [[(String, A.Value)]]
      retrieve a = regroup <$> mapM (sndM execQuery) a
               
      handleSubObj :: (IConnection con) => String -> Integer -> Relation A.Value -> Model con [[(String, Value A.Value)]]
      handleSubObj field id (HasMany idCol r) = recursiveLoad (json2kvp r) (WhereCompiled (printf "%s = %d" idCol id) [])
    
      group :: [(String, Relation A.Value)] -> WhereCompiled -> [([String], Load)]
      group rel (WhereCompiled w vs) = [(fields, Load (printf "SELECT %s FROM %s %s WHERE %s" columns mainTable fulljoins w) vs)]
         where 
            (mainTable, mainKey) = head keys
            fields = map fst $ filter (isColOrKey . snd) rel
            cols = map (unRel . snd) rel
            unRel (IsKey ls) = head $ ls
            unRel (IsCol t c) = (t,c)
            (IsKey keys) = fromJust $ L.find isKey $ map snd rel
            columns = L.intercalate "," [printf "%s.%s" t c | (t,c) <- cols]
            fulljoins = unwords [printf "LEFT OUTER JOIN %s ON %s.%s=%s.%s" t t c mainTable mainKey | (t,c) <- tail keys]

      execQuery :: (IConnection con) => Load -> Model con [[A.Value]] 
      execQuery (Load q vs) = do
         cnn <- ask
         liftIO $ (map.map) sql2aeson <$> withTransaction cnn (\cnn -> quickQuery cnn q (map aeson2sql vs))
        
-- data Load x = LoadW String String Where
--             | LoadV x
--             | LoadR x
--             | LoadN 
--          deriving (Generic, Show, Eq, Functor)

-- instance DB Load Value where
--    optimize = map (pure *** fmap pure)
--    sendSql (LoadW _ _ _) = True
--    sendSql (LoadR _) = True
--    sendSql _ = False
--    sendSqlR (LoadR _) = True
--    sendSqlR _ = False
--    wrapCnst (LoadV a) = Val a
--    wrapCnst LoadN = Null
--    exec (LoadW table column whereClause) = do
--       cnn <- ask
--       v <- liftIO $ withTransaction cnn (\cnn -> quickQuery cnn stmt [])
--       return v 
--       where stmt = printf "SELECT `%s` FROM `%s` WHERE %s" column table whereClause
--    unsafeUnwrap (LoadR x) = x

-- instance Field Load

-- instance Applicative Load where
--    pure = LoadV
--    (LoadV f) <*> (LoadV a) = (LoadV $ f a)
--    (LoadV f) <*> (LoadR a) = (LoadR $ f a)
--    _ <*> _ = LoadN
   
-- load :: (IConnection con, 
--          Typeable a,
--          Show (a Load),
--          ToJSON (a Load),
--          FromJSON (a Value)) => a Load -> Model con [a Value]
-- load = run
