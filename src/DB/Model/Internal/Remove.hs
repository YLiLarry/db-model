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

data Remove = Remove Table Where

remove :: (IConnection con) => Remove -> Model con ()
remove (Remove table whereClause) = do
   cnn <- ask
   void $ liftIO $ withTransaction cnn (\cnn -> quickQuery cnn stmt [])
   where 
      stmt = printf "DELETE FROM `%s` WHERE %s" table whereClause
   
