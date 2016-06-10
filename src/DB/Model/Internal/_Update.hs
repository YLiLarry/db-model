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

module DB.Model.Internal.Update where

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


data Update x = UpdateW String String Where x
              | UpdateR x
              | UpdateN
            deriving (Generic, Show, Eq, Functor)

            
instance Applicative Update where
   pure = UpdateW "" "" ""
   (UpdateW table column w f) <*> (UpdateW _ _ _ a) = (UpdateW table column w $ f a)
   (UpdateW _ _ _ f) <*> (UpdateR a) = (UpdateR $ f a)
   _ <*> _ = UpdateN

instance DB Update Void where
   -- optimize :: [(String, b v)] -> [([String], b [v])] 
   optimize = map combine . groupBy shouldCombine . sortOn tablename where 
      tablename :: (String, Update v) -> String
      tablename (_, UpdateW t _ _ _) = t
      shouldCombine :: (String, Update v) -> (String, Update v) -> Bool
      shouldCombine (_, UpdateW t1 _ _ _) (_, UpdateW t2 _ _ _) = t1 == t2
      combine :: [(String, Update v)] -> ([String], Update [v])
      combine a = (map fst a, UpdateW t c w v) where
         (UpdateW t _ w v) = sequenceA $ map snd a
         c = intercalate "," [ printf "`%s`=?" c | (_, UpdateW _ c _ _) <- a ]
   sendSql (UpdateW _ _ _ _) = True
   sendSql _ = False
   sendSqlR (UpdateR _) = True
   sendSqlR _ = False
   wrapCnst _ = Void
   exec (UpdateW table columns whereClause values) = do
      cnn <- ask
      v <- liftIO $ withTransaction cnn (\cnn -> quickQuery cnn stmt values)
      return v 
      where 
         stmt = printf "UPDATE `%s` SET %s WHERE %s" table columns whereClause
   unsafeUnwrap (UpdateR x) = x

instance Field Update

update :: (IConnection con, 
           Typeable a,
           FromJSON (a Void),
           ToJSON (a Update),
           Generic (a Update),
           Show (a Update)) 
         => a Update -> ModelT con IO ()
update a = void $ run a
