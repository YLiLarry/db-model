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

module DB.Model.Internal.New where

import GHC.Generics hiding (to, from)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Database.HDBC as D hiding (run)
import Text.Printf
import           Data.Aeson (fromJSON, toJSON, FromJSON, ToJSON, GToJSON, GFromJSON)
import qualified Data.Aeson as A
import Control.Arrow
import Data.List as L
import Data.List.Split as L
import           Data.Map (Map)
import qualified Data.Map as M
import DB.Model.Internal.Class
import DB.Model.Internal.Value
import DB.Model.Internal.Exception
import DB.Model.Internal.TypeCast
import qualified Data.Vector as V
import Data.Maybe
import Control.Exception
import GHC.IO.Exception
import Generics.Deriving.Show
import Data.Typeable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import           Data.Proxy (Proxy(..))
import qualified Data.Proxy as P

data New x = New String String x
           | New_ x
           | NewR x
           | NewN
         deriving (Generic, Show, Eq, Functor)

instance Applicative New where
   pure = New "" ""
   (New table column f) <*> (New _ _ a) = (New table column $ f a)
   (New _ _ f) <*> (NewR a) = (NewR $ f a)
   _ <*> _ = NewN
   
instance DB New Value where
   optimize = map combine . groupBy shouldCombine . sortOn tablename where 
      tablename :: (String, New v) -> String
      tablename (_, New t _ _) = t
      shouldCombine :: (String, New v) -> (String, New v) -> Bool
      shouldCombine (_, New t1 _ _) (_, New t2 _ _) = t1 == t2
      combine :: [(String, New v)] -> ([String], New [v])
      combine a = (map fst a, New t c v) where
         (New t _ v) = sequenceA $ map snd a
         c = intercalate "," [ c | (_, New _ c _) <- a ]
   sendSql (New _ _ _) = True
   sendSql (NewR _) = True
   sendSql _ = False
   sendSqlR (NewR _) = True
   sendSqlR _ = False
   wrapCnst (New_ a) = Val a
   wrapCnst _ = Null
   exec (New table columns vals) = do
      cnn <- ask
      liftIO $ withTransaction cnn (\cnn -> quickQuery cnn stmt vals)
      v <- liftIO $ withTransaction cnn (\cnn -> quickQuery cnn "SELECT last_insert_rowid();" [])
      return $ concat <$> replicate (length vals) <$> v 
      where 
         stmt = printf "INSERT INTO `%s` (%s) VALUES (%s)" table columns qqq
         qqq = intercalate "," $ map (const "?") vals
   unsafeUnwrap (NewR a) = a

instance Field New

new :: (IConnection con,
        Typeable a,
        Show (a New),
        ToJSON (a New),
        FromJSON (a Value)) => a New -> Model con (a Value)
new a = head <$> run a

