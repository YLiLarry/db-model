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


data Load = Load String deriving (Show)

instance Query Load where
   group rel w _ = [(fields, Load $ printf "SELECT %s FROM %s %s WHERE %s" columns mainTable fulljoins w)]
      where 
         (mainTable, mainKey) = head keys
         fields = map fst $ filter (isColOrKey . snd) rel
         cols = map (unRel . snd) rel
         unRel (IsKey ls) = head $ ls
         unRel (IsCol t c) = (t,c)
         (IsKey keys) = fromJust $ L.find isKey $ map snd rel
         columns = L.intercalate "," [printf "%s.%s" t c | (t,c) <- cols]
         fulljoins = unwords [printf "LEFT OUTER JOIN %s ON %s.%s=%s.%s" t t c mainTable mainKey | (t,c) <- tail keys]

   execQuery (Load q) = do
      cnn <- ask
      liftIO $ (map.map) sql2aeson <$> withTransaction cnn (\cnn -> quickQuery cnn q [])
        
        
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
