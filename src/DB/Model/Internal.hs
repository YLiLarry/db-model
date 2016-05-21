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
import Data.Maybe
import Control.Exception
import GHC.IO.Exception

class Model (a :: (* -> *) -> *)
-- instance (Field f) => Model (a f)

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
            | Const x
            | ConstNull 
            deriving (Generic, Generic1, Show, Eq, Functor)
            
instance Applicative Load where
   pure = Const
   (Const f) <*> (Const a) = (Const $ f a)
   _ <*> _ = ConstNull
   
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
throughMaybe f = maybe2value . fmap f . value2maybe 
   
value2maybe :: Value a -> Maybe a
value2maybe (Value x) = Just x
value2maybe Null = Nothing

maybe2value :: Maybe a -> Value a
maybe2value Nothing = Null
maybe2value (Just x) = Value x
   
data LastID x = ID Int 
              | Ignored
            deriving (Generic, Generic1, Show, Eq)

-- data Delete x = Delete String String ID
              

class (Functor b, A.FromJSON (b A.Value)) => DB b r | b -> r, r -> b where
   fromSqlVal :: r SqlValue -> [(String, A.Value)]
   wrapVal :: SqlValue -> r SqlValue
   wrapCnst :: b SqlValue -> r SqlValue
   sendSql :: b SqlValue -> Bool
   optimize :: [(String, b SqlValue)] -> [([String], b [SqlValue])] 

   to :: (A.ToJSON (a b)) => a b -> [(String, b SqlValue)]
   to = map ((fmap aeson2sql . unsafeFromJSON) `second`) . M.toList . unsafeFromJSON . A.toJSON

   from :: (A.FromJSON (a r)) => [(String, r SqlValue)] -> a r
   from = unsafeFromJSON . A.toJSON . M.fromList . map ((M.fromList . fromSqlVal) `second`)

   runSql :: (IConnection cnn) => [(String, b SqlValue)] -> ReaderT cnn IO [[(String, r SqlValue)]]
   runSql a = mapReaderT (>>= validate) $ map toR <$> retag <$> mapM (runKleisli . second . Kleisli $ exec) (optimize nonconstants)
      
      where
         (nonconstants, constants) = partition (sendSql . snd) a
         toR :: [(String, SqlValue)] -> [(String, r SqlValue)]
         toR a = map (wrapVal `second`) a ++ map (wrapCnst `second`) constants
         retag :: [([String], [[SqlValue]])] -> [[(String, SqlValue)]]
         retag v = trans [ ((,) prop) <$> vals | (prop, vals) <- v' ]
            where v' = concat [ zip prop (trans matrix) | (prop, matrix) <- v ]
         validate :: [[(String, r SqlValue)]] -> IO [[(String, r SqlValue)]]
         validate matrix
            | isJust $ size matrix = return $ matrix
            | otherwise = fail $ printf "Retrived records have different length.\n" 
   
   exec :: (IConnection cnn) => b [SqlValue] -> ReaderT cnn IO [[SqlValue]]

   run :: (A.ToJSON (a b), A.FromJSON (a r), IConnection cnn, Show (a b)) => a b -> ReaderT cnn IO [a r]
   run a = (map from <$> (handle report `mapReaderT` (runSql $ to a)))
      where 
         report :: IOException -> IO a
         report e = fail $ printf "IO error in (run %s):\n\t%s" (show a) (ioe_description e)

unsafeFromJSON :: A.FromJSON a => A.Value -> a
unsafeFromJSON a = 
   case A.fromJSON a of
      A.Error m -> error $ printf "Error when converting %s:\n%s" (show a) m
      A.Success a -> a


instance DB Save LastID where
   optimize = map combine . groupBy shouldCombine where 
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
   optimize = map (pure *** fmap pure)
   sendSql (Load _ _ _) = True
   sendSql _ = False
   wrapVal = Value
   wrapCnst (Const a) = Value a
   wrapCnst ConstNull = Null
   fromSqlVal (Value x) = [("tag", A.toJSON "Value"), ("contents", sql2aeson x)]
   fromSqlVal Null = [("tag", A.toJSON "Null"), ("contents", A.toJSON ([] :: [()]))]
   exec (Load table column whereClause) = do
      cnn <- ask
      v <- lift $ withTransaction cnn (\cnn -> quickQuery cnn stmt [])
      return v 
      where stmt = printf "SELECT `%s` FROM `%s` WHERE %s" column table whereClause

class Matrix m where
   size :: m (m a) -> Maybe (Int, Int)
   trans  :: m (m a) -> m (m a)

instance Matrix [] where
   trans  = transpose
   size [] = Just (0,0)
   size [[]] = Nothing
   size matrix
      | all (== fstRow) (map length matrix) = Just (length matrix,fstRow)
      | otherwise = Nothing
      where fstRow  = length $ head matrix
   
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
      b = Const 3,
      c = ConstNull
   }
   
   load test :: [Test Value]
   
   test2 :: Test Save {
      a = Save "Table" "Column" 4,
      b = Replace "Table" "Column" 4
   }
   
@
-}
