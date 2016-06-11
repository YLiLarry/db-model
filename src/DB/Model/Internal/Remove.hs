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

module DB.Model.Internal.Remove where
   
import DB.Model.Internal.Prelude
import DB.Model.Internal.Class
import DB.Model.Internal.Value
import Control.Monad.Reader (ask)

import           Database.HDBC (IConnection, withTransaction, quickQuery)
import qualified Database.HDBC as H
import           Data.Aeson (fromJSON, toJSON, FromJSON, ToJSON, GToJSON, GFromJSON)
import qualified Data.Aeson as A


nonRecursiveRemove :: (IConnection con) => [(String, Relation A.Value)] -> [(String, A.Value)] -> Model con ()
nonRecursiveRemove rel val = do
   deleteFromTable False keyVal $ head keys
   mapM_ (deleteFromTable False keyVal) $ tail keys
   where 
      (kField, IsKey keys) = findIsKey rel
      keyVal = findKeyVal rel val
   
deleteFromTable :: (IConnection con) => Bool -> Integer -> (Table, Column) -> Model con ()
deleteFromTable mustExist keyVal (table, keyCol) = do
   cnn <- ask
   modified <- liftIO $ withTransaction cnn 
      (\cnn -> run cnn stmt [toSql keyVal])
   when (mustExist && modified == 0) $ throwError 
      $ printf "Could not find a row to delete when executing 'DELETE FROM %s WHERE %s=%d'."
         table keyCol keyVal
   where
      stmt = printf "DELETE FROM %s WHERE %s=?" table keyCol
