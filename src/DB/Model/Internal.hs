{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

module DB.Model.Internal where

import GHC.Generics hiding (to, from)
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Database.HDBC as D hiding (run)
import Text.Printf
import qualified Data.Aeson as A
import Control.Arrow
import Data.List as L
import Data.List.Split as L
import qualified Data.Map as M
import DB.Model.Internal.Exception
import DB.Model.Internal.TypeCast
import qualified Data.Vector as V
      
class Model (a :: (* -> *) -> *)
instance (Field f) => Model (a f)

instance (Model a, Generic (a m), A.GToJSON (Rep (a m))) => A.ToJSON (a m)
instance (Model a, Generic (a m), A.GFromJSON (Rep (a m))) => A.FromJSON (a m)

class Field (f :: * -> *)
instance Field Load
instance Field Value
instance Field Save
instance Field LastID

instance (Field f, Generic (f x), A.GToJSON (Rep (f x))) => A.ToJSON (f x)
instance (Field f, Generic (f x), A.GFromJSON (Rep (f x))) => A.FromJSON (f x)


data Load x = Load String String String
            | SetVal x
            | SetNull 
            deriving (Generic, Generic1, Show, Eq, Functor)
            
instance Applicative Load where
   pure = SetVal
   (SetVal f) <*> (SetVal a) = (SetVal $ f a)
   _ <*> _ = SetNull
   
data Save x = Save String String x
            | Ignore
            deriving (Generic, Generic1, Show, Eq, Functor)

instance Applicative Save where
   pure = Save "" ""
   (Save table column f) <*> (Save _ _ a) = (Save table column $ f a)
   _ <*> _ = Ignore
   
data Value x = Value x
             | Null
            deriving (Generic, Generic1, Show, Eq, Functor)

instance Applicative Value where
   pure = Value
   (Value f) <*> (Value a) = (Value $ f a)
   _ <*> _ = Null
            
throughMaybe :: (a -> b) -> Value a -> Value b
throughMaybe f = fromMaybe . fmap f . toMaybe 
   
toMaybe :: Value a -> Maybe a
toMaybe (Value x) = Just x
toMaybe Null = Nothing

fromMaybe :: Maybe a -> Value a
fromMaybe Nothing = Null
fromMaybe (Just x) = Value x
   
data LastID x = ID Int 
              | Ignored
            deriving (Generic, Generic1, Show, Eq)

class (Functor b, A.FromJSON (b A.Value), Show (b [SqlValue])) => DB b r | b -> r, r -> b where
   fromSqlVal :: r SqlValue -> [(String, A.Value)]
   wrapVal :: SqlValue -> r SqlValue
   wrapCnst :: b SqlValue -> r SqlValue
   sendSql :: b SqlValue -> Bool
   adjust :: [(String, b SqlValue)] -> [([String], b [SqlValue])] 

   to :: (A.ToJSON (a b)) => a b -> [(String, b SqlValue)]
   to = map ((fmap aeson2sql . unsafeFromJSON) `second`) . M.toList . unsafeFromJSON . A.toJSON

   from :: (A.FromJSON (a r)) => [(String, r SqlValue)] -> a r
   from = unsafeFromJSON . A.toJSON . M.fromList . map ((M.fromList . fromSqlVal) `second`)

   run :: (IConnection cnn) => [(String, b SqlValue)] -> ReaderT cnn IO [[(String, r SqlValue)]]
   run a = map g <$> f <$> mapM (runKleisli . second . Kleisli $ exec) (adjust nonconstants)
      where
         (nonconstants, constants) = map (wrapCnst `second`) `second` partition (sendSql . snd) a
         g :: [(String, SqlValue)] -> [(String, r SqlValue)]
         g a = map (wrapVal `second`) a ++ constants
         f :: [([String], [[SqlValue]])] -> [[(String, SqlValue)]]
         f v = trans [ ((,) prop) <$> vals | (prop, vals) <- v' ]
            where v' = concat [ zip prop (trans matrix) | (prop, matrix) <- v ]
   
   exec :: (IConnection cnn) => b [SqlValue] -> ReaderT cnn IO [[SqlValue]]


unsafeFromJSON :: A.FromJSON a => A.Value -> a
unsafeFromJSON a = 
   case A.fromJSON a of
      A.Error m -> error $ printf "Error when converting %s:\n%s" (show a) m
      A.Success a -> a


instance DB Save LastID where
   adjust = map combine . groupBy shouldCombine where 
      shouldCombine :: (String, Save SqlValue) -> (String, Save SqlValue) -> Bool
      shouldCombine (_, Save t1 _ _) (_, Save t2 _ _) = t1 == t2
      combine :: [(String, Save SqlValue)] -> ([String], Save [SqlValue])
      combine a = (map fst a, Save t c v) where
         (Save t _ v) = sequenceA $ map snd a
         c = intercalate "," [ c | (_, Save _ c _) <- a ]
      
   sendSql (Save _ _ _) = True
   sendSql _ = False
   wrapVal = ID . D.fromSql
   wrapCnst _ = Ignored
   fromSqlVal (ID x) = [("tag", A.toJSON "ID"), ("contents", A.toJSON x)]
   fromSqlVal Ignored = [("tag", A.toJSON "Ignored"), ("contents", A.toJSON ([] :: [()]))]
   exec (Save table column vals) = do
      cnn <- ask
      liftIO $ withTransaction cnn (\cnn -> quickQuery cnn stmt vals)
      v <- liftIO $ withTransaction cnn (\cnn -> quickQuery cnn "SELECT last_insert_rowid();" [])
      return $ concat <$> replicate (length vals) <$> v 
      where 
         stmt = printf "INSERT INTO `%s` (%s) VALUES (%s)" table column qqq
         qqq = intercalate "," $ map (const "?") vals
   
instance DB Load Value where
   adjust = map (pure *** fmap pure)
   sendSql (Load _ _ _) = True
   sendSql _ = False
   wrapVal = Value
   wrapCnst (SetVal a) = Value a
   wrapCnst SetNull = Null
   fromSqlVal (Value x) = [("tag", A.toJSON "Value"), ("contents", sql2aeson x)]
   fromSqlVal Null = [("tag", A.toJSON "Null"), ("contents", A.toJSON ([] :: [()]))]
   exec (Load table column whereClause) = do
      cnn <- ask
      v <- lift $ withTransaction cnn (\cnn -> quickQuery cnn stmt [])
      return v 
      where stmt = printf "SELECT `%s` FROM `%s` WHERE %s" column table whereClause

class Matrix m where
   verify :: m (m a) -> Maybe (Int, Int)
   trans  :: m (m a) -> m (m a)

instance Matrix [] where
   trans  = transpose
   verify [] = Just (0,0)
   verify [[]] = Nothing
   verify l@[a]
      | all (== fstRow) (map length l) = Just (length l,fstRow)
      | otherwise = Nothing
      where fstRow  = length a
   
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
      b = SetVal 3,
      c = SetNull
   }
   
   load test :: [Test Value]
   
   test2 :: Test Save {
      a = Save "Table" "Column" 4,
      b = Replace "Table" "Column" 4
   }
   
@
-}
