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

module DB.Model.Internal.Class where

import DB.Model.Internal.Prelude
import DB.Model.Internal.Value

import qualified Data.Aeson as A
import qualified Data.List as L

newtype ModelT r m a = ModelT { unModelT :: ReaderT r (ExceptT String m) a} 
deriving instance (Functor m) => Functor (ModelT r m)
deriving instance (Monad m) => Applicative (ModelT r m)
deriving instance (Monad m) => Monad (ModelT r m)
deriving instance (Monad m) => MonadError String (ModelT r m)
deriving instance (Monad m) => MonadReader r (ModelT r m)
deriving instance (MonadIO m) => MonadIO (ModelT cnn m)
deriving instance (MonadState s m) => MonadState s (ModelT r m)
deriving instance (Monad m) => Alternative (ModelT cnn m)
deriving instance (MonadPlus m) => MonadPlus (ModelT cnn m)
instance MonadTrans (ModelT r) where
   lift = ModelT . lift . lift
   
type Model r = ModelT r IO

runModelT :: ModelT r m a -> r -> m (Either String a)
runModelT a con = runExceptT $ runReaderT (unModelT a) con

mapModelT :: (m (Either String a) -> n (Either String b)) -> ModelT r m a -> ModelT r n b
mapModelT f x = ModelT $ mapReaderT (mapExceptT f) $ unModelT x

type Query = String
type Table = String
type Column = String
type JSONValue = A.Value

data Relation t = IsCol Table Column -- ^ Table name, column name, key column name in the table used for this object
                | IsKey [(Table, Column)] -- ^ Table name, column name. Current field is a key. 
                | HasMany Column t
                | IsConst t
                | IsNull
               deriving (Generic)

instance (FromJSON x) => FromJSON (Relation x)
instance (ToJSON x) => ToJSON (Relation x)
instance (GShow x) => GShow (Relation x)
instance (GEq x) => GEq (Relation x)

regroup :: [([String], [[A.Value]])] -> [[(String, A.Value)]]
regroup v = L.transpose [ ((,) prop) <$> vals | (prop, vals) <- v' ]
   where v' = concat [ zip prop (L.transpose  matrix) | (prop, matrix) <- v ]

isConst :: Relation v -> Bool
isConst (IsConst _) = True
isConst _ = False    

isNull :: Relation v -> Bool
isNull IsNull = True
isNull _ = False     
    
isHasMany :: Relation v -> Bool
isHasMany (HasMany _ _) = True
isHasMany _ = False    

isKey :: Relation v -> Bool
isKey (IsKey _) = True
isKey _ = False
                  
isColOrKey :: Relation v -> Bool
isColOrKey (IsKey _) = True
isColOrKey (IsCol _ _) = True
isColOrKey _ = False

getConst :: Relation A.Value -> A.Value
getConst (IsConst v) = v
  
findIsKey :: [(String, Relation A.Value)] -> (String, Relation A.Value)
findIsKey = fromJust . L.find (isKey . snd)
  
isKeyField :: [(String, Relation A.Value)] -> String
isKeyField = fst . findIsKey

findKeyVal :: [(String, Relation A.Value)] -> [(String, A.Value)] -> Integer
findKeyVal r o = x
   where (Val x) = unsafeTo $ fromJust $ L.lookup (isKeyField r) o 

relForField :: String -> [(String, Relation A.Value)] -> Relation A.Value
relForField field rel = fromJust $ L.lookup field rel

