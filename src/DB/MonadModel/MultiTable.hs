{-|
Module      : DB.MonadModel.MultiTable
Description : A monad transformer interface
Copyright   : (c) Yu Li
License     : MIT
Maintainer  : ylilarry@gmail.com
Stability   : experimental
Portability : GHC extensions

This is a monad transformer interface to the "DB.Model.MultiTable" module
-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module DB.MonadModel.MultiTable
   (module X,
    load,
    new,
    update,
    remove,
    MonadModel(..))
   where
   
import DB.Model.Internal.Prelude
import DB.Model.MultiTable as X hiding (load, new, remove, update, loadR, newR, removeR, updateR)
import qualified DB.Model.MultiTable as M


class (IConnection c) => MonadModel c m | m -> c where
   fromModel :: Model c a -> m a
   
   load :: (MultiTable a) => Where a -> m [a Value]
   load = fromModel . M.load

   new :: (MultiTable a) => a Value -> m (a Value)
   new = fromModel . M.new

   update :: (MultiTable a) => a Value -> m ()
   update = fromModel . M.update

   remove :: (MultiTable a) => a Value -> m ()
   remove = fromModel . M.remove

   loadR :: (MultiTable a) => a Relation -> Where a -> m [a Value]
   loadR r = fromModel . M.loadR r
   
   updateR :: (MultiTable a) => a Relation -> a Value -> m ()
   updateR r = fromModel . M.updateR r
   
   newR :: (MultiTable a) => a Relation -> a Value -> m (a Value)
   newR r = fromModel . M.newR r
   
   removeR :: (MultiTable a) => a Relation -> a Value -> m ()
   removeR r = fromModel . M.removeR r


instance (IConnection c) => MonadModel c (ModelT c IO) where
   fromModel = id
   
   