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

import GHC.Generics hiding (to, from)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Database.HDBC as D hiding (run)
import Text.Printf
import           Data.Aeson (fromJSON, toJSON, FromJSON, ToJSON, GToJSON, GFromJSON)
import qualified Data.Aeson as A
import Control.Arrow
import qualified Data.List as L
import Data.List.Split as L
import           Data.Map (Map)
import qualified Data.Map as M
import DB.Model.Internal.Prelude
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

newtype ModelT r m a = ModelT { unModelT :: ReaderT r (ExceptT String m) a} 
deriving instance (Monad m) => Functor (ModelT r m)
deriving instance (Monad m) => Applicative (ModelT r m)
deriving instance (Monad m) => Monad (ModelT r m)
deriving instance (Monad m) => MonadError String (ModelT r m)
deriving instance (Monad m) => MonadReader r (ModelT r m)
deriving instance (MonadIO m) => MonadIO (ModelT cnn m)
type Model r = ModelT r IO

runModelT :: ModelT r m a -> r -> m (Either String a)
runModelT a con = runExceptT $ runReaderT (unModelT a) con

-- data Query = Query String [A.Value]
type Where = String
type Table = String
type Column = String


data Relation t = IsCol Table Column -- ^ Table name, column name, key column name in the table used for this object
                | IsKey [(Table, Column)] -- ^ Table name, column name. Current field is a key. 
                | HasMany Column t
                | IsConst t
                | IsNull
               deriving (Generic)

instance (FromJSON x) => FromJSON (Relation x)
instance (ToJSON x) => ToJSON (Relation x)
instance (GShow x) => GShow (Relation x)
deriving instance (Show x) => Show (Relation x)

class (Show a) => Query a where
   -- ^ This function should only consider IsCol, IsKey and Val. Other cases should be filtered before this function is called.
   group :: [(String, Relation A.Value)] -> Where -> [(String, A.Value)] -> [([String], a)]
   
   execQuery :: (IConnection con) => a -> Model con [[A.Value]] 
      
   recursive :: (IConnection con) => Proxy a -> [(String, Relation A.Value)] -> Where -> [(String, A.Value)] -> Model con [[(String, Value A.Value)]]
   recursive _ r w v = do
      let nulls  = map (const Null `second`) $ filter (isNull . snd) r
      let consts  = map (getConst `second`) $ filter (isConst . snd) r
      let hasMany = filter (isHasMany . snd) r
      let others  = filter (isColOrKey . snd) r
      result <- retrieve (group others w v)
      let ids = [ findKeyVal r o | o <- result ]
      recurs <- sequence [ sequence [ sequence (field, Many <$> map kvp2json <$> (handleSubObj field (unsafeTo id) r v)) | (field, r) <- hasMany] | id <- ids ]
      return $ zipWith (++) recurs [ nulls ++ map (wrapValue `second`) (consts ++ object) | object <- result ]
      where
         wrapValue :: A.Value -> Value A.Value
         wrapValue A.Null = Null
         wrapValue x = Val x
         
         retrieve :: (IConnection con) => [([String], a)] -> Model con [[(String, A.Value)]]
         retrieve a = regroup <$> mapM (sndM execQuery) a   
                  
         handleSubObj :: (IConnection con) => String -> Integer -> Relation A.Value -> [(String, A.Value)] -> Model con [[(String, Value A.Value)]]
         handleSubObj field id (HasMany idCol r) v = recursive (Proxy :: Proxy a) (json2kvp r) (printf "%s = %d" idCol id) val
            where val = if null v then [] else json2kvp $ fromJust $ L.lookup field v


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

findKeyVal :: [(String, Relation A.Value)] -> [(String, A.Value)] -> A.Value
findKeyVal r o = fromJust $ L.lookup (isKeyField r) o 

findForField :: String -> [(String, Relation A.Value)] -> Relation A.Value
findForField field rel = fromJust $ L.lookup field rel

-- instance ToJSON SqlValue where
--    toJSON = sql2aeson
-- instance FromJSON SqlValue where
--    parseJSON = return . aeson2sql


-- class (Functor f, 
--        Typeable f, 
--        Generic (f A.Value), 
--        Generic (f SqlValue),
--        GToJSON (Rep (f A.Value)),
--        GToJSON (Rep (f SqlValue)),
--        GFromJSON (Rep (f A.Value)),
--        GFromJSON (Rep (f SqlValue))) => Field (f :: * -> *)

-- instance {-# INCOHERENT #-} (Field f, Generic (f x), GToJSON (Rep (f x))) => ToJSON (f x)
-- instance {-# INCOHERENT #-} (Field f, Generic (f x), GFromJSON (Rep (f x))) => FromJSON (f x)
-- instance {-# INCOHERENT #-} (Field f, Generic (f x), GShow' (Rep (f x))) => GShow (f x)

-- class (Field w) => WrapVal w where
--    wrapVal :: A.Value -> w A.Value
--    wrapVals :: [A.Value] -> w A.Value

   
-- class (Field b, WrapVal r) => DB b r | b -> r where
--    wrapCnst :: b v -> r v
--    sendSql :: b x -> Bool
--    sendSqlR :: b x -> Bool
--    optimize :: [(String, b v)] -> [([String], b [v])] 
--    unsafeUnwrap :: (Show v) => b v -> v

--    aeson2kvp :: A.Value -> [(String, b A.Value)]
--    aeson2kvp = M.toList . unsafeTo

--    toDB :: (ToJSON (a b), IConnection cnn) => a b -> Model cnn [(String, b A.Value)]
--    toDB = return . aeson2kvp . toJSON

--    runSql :: (IConnection cnn) => [(String, b A.Value)] -> Model cnn [[(String, r A.Value)]]
--    runSql a = do
--       recurisveResults <- mapM (sndM loadR) recurisve
--       loadonceResults <- retag <$> mapM (sndM exec) (optimize $ map (fmap aeson2sql `second`) loadonce)
--       let constantResults = (map (wrapCnst `second`) constants) ++ recurisveResults :: [(String, r A.Value)]
--       validate $ map (addConsts constantResults) loadonceResults
--       where
--          (nonconstants, constants) = partition (sendSql . snd) a
--          (recurisve, loadonce) = partition (sendSqlR . snd) nonconstants
--          loadR :: (IConnection cnn) => b A.Value -> Model cnn (r A.Value)
--          loadR v = do 
--             r <- runSql $ (aeson2kvp $ unsafeUnwrap v :: [(String, b A.Value)])
--             return $ wrapVals $ map kvp2aeson r
--          addConsts :: [(String, r A.Value)] -> [(String, SqlValue)] -> [(String, r A.Value)]
--          addConsts consts a = map ((wrapVal . sql2aeson) `second`) a ++ consts
--          retag :: [([String], [[SqlValue]])] -> [[(String, SqlValue)]]
--          retag v = trans [ ((,) prop) <$> vals | (prop, vals) <- v' ]
--             where v' = concat [ zip prop (trans matrix) | (prop, matrix) <- v ]
--          validate :: (IConnection cnn) => [[(String, r A.Value)]] -> Model cnn [[(String, r A.Value)]]
--          validate matrix
--             | isJust $ size matrix = return $ matrix
--             | otherwise = throwError $ printf "Retrieved records in inconsistent length.\n" 
   
--    exec :: (IConnection cnn) => b [SqlValue] -> Model cnn [[SqlValue]]

--    run :: (ToJSON (a b), FromJSON (a r), IConnection cnn, Show (a b), Typeable (a r)) => a b -> Model cnn [a r]
--    run a = map (unsafeTo . kvp2aeson) <$> (runSql =<< toDB a)
   

-- class Matrix m where
--    size :: m (m a) -> Maybe (Int, Int)
--    trans  :: m (m a) -> m (m a)

-- instance Matrix [] where
--    trans  = transpose
--    size [] = Just (0,0)
--    size [[]] = Nothing
--    size matrix
--       | all (== fstRow) (map length matrix) = Just (length matrix,fstRow)
--       | otherwise = Nothing
--       where fstRow  = length $ head matrix

-- kvp2aeson :: (Field r) => [(String, r A.Value)] -> A.Value
-- kvp2aeson = toJSON . M.fromList



            
-- -- throughMaybe :: (a -> b) -> Value a -> Value b
-- -- throughMaybe f = maybe2value . fmap f . value2maybe 
   
-- -- value2maybe :: Value a -> Maybe a
-- -- value2maybe (Value x) = Just x
-- -- value2maybe ValueN = Nothing

-- -- maybe2value :: Maybe a -> Value a
-- -- maybe2value Nothing = ValueN
-- -- maybe2value (Just x) = Value x



-- data Void x = Void deriving (Generic, Show, Eq, Functor)
   
-- instance WrapVal Void where
--    wrapVal  _ = Void
--    wrapVals _ = Void

-- instance Field Void

