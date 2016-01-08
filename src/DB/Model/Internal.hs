{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module DB.Model.Internal where

import GHC.Generics
import Control.Monad
import Data.IORef
import Cache.State
import Control.Monad.State
import Control.Monad.Reader
import Data.Hashable
import Database.HDBC
import Text.Printf

data Select = Select String String String

instance DB Select SqlValue where
   execNoCache (Select table column whereClause) = do
      cnn <- ask
      v <- lift $ withTransaction cnn (\cnn -> quickQuery cnn (printf "SELECT * FROM %s WHERE %s" table whereClause) [])
      return v
      
         
      

class DB c v where
   exec :: (Hashable c, Eq c, IConnection cnn) => c -> StateT (Cache c [v]) (ReaderT cnn IO) v
   exec c = head <$> withCache execNoCache c

   execNoCache :: (IConnection cnn) => c -> ReaderT cnn IO [v]
   

class Run a where
   run :: (DB c v, IConnection cnn) => a c -> StateT (Cache c [v]) (ReaderT cnn IO) [a v]
   default run :: (DB c v, IConnection cnn,
                   GRun (Rep (a c)) (Rep (a v)) c v,
                   Generic (a v),
                   Generic (a c))
                  => a c -> StateT (Cache c [v]) (ReaderT cnn IO) [a v]
   run a = to <$> gRun (from a)


class GRun c' v' c v where
   gRun :: (IConnection cnn) => c' p -> StateT (Cache c [v]) (ReaderT cnn IO) (v' p)

instance GRun U1 U1 c v where
   gRun _ = return U1

instance (GRun c' v' c v) => GRun (M1 x y c') (M1 x y v') c v where
   gRun (M1 c) = M1 <$> gRun c 

instance (GRun c' v' c v, GRun cs' vs' c v) => GRun (c' :*: cs') (v' :*: vs') c v where
   gRun (c :*: cs) = gRun c `mult` gRun cs
      where mult = liftM2 (:*:)

instance (GRun c' v' c v, GRun cs' vs' c v) => GRun (c' :+: cs') (v' :+: vs') c v where
   gRun (L1 c) = L1 <$> gRun c
   gRun (R1 c) = R1 <$> gRun c

instance (DB c v, Eq c, Hashable c) => GRun (K1 x c) (K1 x v) c v where
   gRun (K1 c) = K1 <$> exec c

{- 
@

   data Test m = Test {
      a :: m String,
      b :: m Int,
      c :: m Int
   }

   test :: Test Load
   test = Test {
      a = Load "Table" "Column" "id > 5",
      b = LVal 3,
      c = LNull
   }
   
   load test :: [Test Value]
   
   test2 :: Test Save {
      a = Save "Table" "Column" 4,
      b = Replace "Table" "Column" 4
   }
   
@
-}

class (Run a) => Save a v c s where
   save :: a v -> s -> a c

class (Run a) => Load a v c s where
   load :: a v -> s -> a c

class (Run a) => Delete a v c s where
   delete :: a v -> s -> a c

