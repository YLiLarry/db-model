{-|
Module      : DB.MonadModel.SimpleTable
Description : A monad transformer interface 
Copyright   : (c) Yu Li
License     : MIT
Maintainer  : ylilarry@gmail.com
Stability   : experimental
Portability : GHC extensions

This is a monad transformer interface to the "DB.Model.SimpleTable" module
-}


module DB.MonadModel.SimpleTable 
   (SimpleTable(..), 
    Relation(..),
    module X) 
   where
   
import DB.MonadModel.MultiTable as X hiding (Relation(..), relation)
import DB.Model.Internal.SimpleTable

