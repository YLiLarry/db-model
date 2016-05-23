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

module DB.Model.Internal where

import GHC.Generics hiding (to, from)
import Control.Monad
import Control.Monad.State
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
import DB.Model.Internal.Exception
import DB.Model.Internal.TypeCast
import qualified Data.Vector as V
import Data.Maybe
import Control.Exception
import GHC.IO.Exception
import Generics.Deriving.Show
import Data.Typeable

class Model (a :: (* -> *) -> *)
instance {-# OVERLAPPABLE #-} (Model a, Generic (a m), GToJSON (Rep (a m))) => ToJSON (a m)
instance {-# OVERLAPPABLE #-} (Model a, Generic (a m), GFromJSON (Rep (a m))) => FromJSON (a m)
instance {-# OVERLAPPABLE #-} (Model x, Generic (x a), GShow' (Rep (x a))) => GShow (x a)


class (Functor f, 
       Typeable f, 
       Generic (f A.Value), 
       Generic (f SqlValue),
       GToJSON (Rep (f A.Value)),
       GToJSON (Rep (f SqlValue)),
       GFromJSON (Rep (f A.Value)),
       GFromJSON (Rep (f SqlValue))) => Field (f :: * -> *)

instance Field Load
instance Field Value
instance Field Save
instance Field Key

instance {-# OVERLAPPABLE #-} (Field f, Generic (f x), GToJSON (Rep (f x))) => ToJSON (f x)
instance {-# OVERLAPPABLE #-} (Field f, Generic (f x), GFromJSON (Rep (f x))) => FromJSON (f x)
instance {-# OVERLAPPABLE #-} (Field f, Generic (f x), GShow' (Rep (f x))) => GShow (f x)

instance ToJSON SqlValue where
   toJSON = sql2aeson
instance FromJSON SqlValue where
   parseJSON = return . aeson2sql

data Load x = Load String String String
            | LoadV x
            | LoadR x
            | LoadN 
            deriving (Generic, Generic1, Show, Eq, Functor)
            
instance Applicative Load where
   pure = LoadV
   (LoadV f) <*> (LoadV a) = (LoadV $ f a)
   (LoadV f) <*> (LoadR a) = (LoadR $ f a)
   _ <*> _ = LoadN
   
data Save x = Save String String x
            | Save_ x
            | SaveR x
            | SaveN
            deriving (Generic, Generic1, Show, Eq, Functor)

instance Applicative Save where
   pure = Save "" ""
   (Save table column f) <*> (Save _ _ a) = (Save table column $ f a)
   (Save table column f) <*> (SaveR a) = (SaveR $ f a)
   _ <*> _ = SaveN
   
data Value x = Value x
             | Values [x]
             | ValueN
            deriving (Generic, Generic1, Show, Eq, Functor)

instance Applicative Value where
   pure = Value
   (Value f) <*> (Value a) = (Value $ f a)
   (Value f) <*> (Values a) = (Values $ map f a)
   _ <*> _ = ValueN
            
throughMaybe :: (a -> b) -> Value a -> Value b
throughMaybe f = maybe2value . fmap f . value2maybe 
   
value2maybe :: Value a -> Maybe a
value2maybe (Value x) = Just x
value2maybe ValueN = Nothing

maybe2value :: Maybe a -> Value a
maybe2value Nothing = ValueN
maybe2value (Just x) = Value x
   
data Key x = Key Integer
           | Key_ x
           | Keys [x]
           | KeyN
         deriving (Generic, Generic1, Show, Eq, Functor)
         
data Delete x = Delete String String String 
              | DeleteN
            deriving (Generic, Generic1, Show, Eq, Functor)

data None x = None deriving (Generic, Generic1, Show, Eq, Functor)

class (Field b, Field r) => DB b r | b -> r, r -> b where
   fromSqlVal :: r SqlValue -> [(String, A.Value)]
   wrapCnst :: b v -> r v
   wrapVal :: A.Value -> r A.Value
   wrapVals :: [A.Value] -> r A.Value
   sendSql :: b x -> Bool
   sendSqlR :: b x -> Bool
   optimize :: [(String, b v)] -> [([String], b [v])] 
   unsafeUnwrap :: (Show v) => b v -> v

   aeson2kvp :: A.Value -> [(String, b A.Value)]
   aeson2kvp = M.toList . unsafeFromJSON
   
   kvp2aeson :: [(String, r A.Value)] -> A.Value
   kvp2aeson = toJSON . M.fromList

   toDB :: (ToJSON (a b), IConnection cnn) => a b -> ReaderT cnn IO [(String, b A.Value)]
   toDB = return . aeson2kvp . toJSON

   fromDB :: (FromJSON (a r), Typeable (a r)) => [(String, r A.Value)] -> a r
   fromDB = unsafeFromJSON . kvp2aeson

   runSql :: (IConnection cnn) => [(String, b A.Value)] -> ReaderT cnn IO [[(String, r A.Value)]]
   runSql a = do
      recurisveResults <- mapM (sndM loadR) recurisve
      loadonceResults <- retag <$> mapM (sndM exec) (optimize $ map (fmap aeson2sql `second`) loadonce)
      let constantResults = (map (wrapCnst `second`) constants) ++ recurisveResults :: [(String, r A.Value)]
      liftIO $ validate $ map (addConsts constantResults) loadonceResults
      where
         (nonconstants, constants) = partition (sendSql . snd) a
         (recurisve, loadonce) = partition (sendSqlR . snd) nonconstants
         loadR :: (IConnection cnn) => b A.Value -> ReaderT cnn IO (r A.Value)
         loadR v = do 
            r <- runSql $ (aeson2kvp $ unsafeUnwrap v :: [(String, b A.Value)])
            return $ wrapVals $ map kvp2aeson r
         addConsts :: [(String, r A.Value)] -> [(String, SqlValue)] -> [(String, r A.Value)]
         addConsts consts a = map ((wrapVal . sql2aeson) `second`) a ++ consts
         retag :: [([String], [[SqlValue]])] -> [[(String, SqlValue)]]
         retag v = trans [ ((,) prop) <$> vals | (prop, vals) <- v' ]
            where v' = concat [ zip prop (trans matrix) | (prop, matrix) <- v ]
         validate :: [[(String, r A.Value)]] -> IO [[(String, r A.Value)]]
         validate matrix
            | isJust $ size matrix = return $ matrix
            | otherwise = fail $ printf "Retrieved records in inconsistent length.\n" 
   
   exec :: (IConnection cnn) => b [SqlValue] -> ReaderT cnn IO [[SqlValue]]

   run :: (ToJSON (a b), FromJSON (a r), IConnection cnn, GShow (a b), Typeable (a r)) => a b -> ReaderT cnn IO [a r]
   run a = (map fromDB <$> (handle report `mapReaderT` (runSql =<< toDB a)))
      where 
         report :: IOException -> IO a
         report e = fail $ printf "IO error in (run %s):\n\t%s" (gshow a) (ioe_description e)

unsafeFromJSON :: (Typeable a, FromJSON a) => A.Value -> a
unsafeFromJSON a = 
   case r of
      A.Error m -> error $ printf "Error when converting %s to %s:\n%s" (show a) (show $ typeOf r) m
      A.Success a -> a
   where r = fromJSON a


instance DB Save Key where
   optimize = map combine . groupBy shouldCombine where 
      shouldCombine :: (String, Save v) -> (String, Save v) -> Bool
      shouldCombine (_, Save t1 _ _) (_, Save t2 _ _) = t1 == t2
      combine :: [(String, Save v)] -> ([String], Save [v])
      combine a = (map fst a, Save t c v) where
         (Save t _ v) = sequenceA $ map snd a
         c = intercalate "," [ c | (_, Save _ c _) <- a ]
   sendSql (Save _ _ _) = True
   sendSql (SaveR _) = True
   sendSql _ = False
   sendSqlR (SaveR _) = True
   sendSqlR _ = False
   wrapCnst (Save_ a) = Key_ a
   wrapCnst _ = KeyN
   wrapVal = Key . unsafeFromJSON
   wrapVals = Keys
   fromSqlVal (Key x) = [("tag", toJSON "Key"), ("contents", toJSON x)]
   fromSqlVal KeyN = [("tag", toJSON "KeyN"), ("contents", toJSON ([] :: [()]))]
   exec (Save table column vals) = do
      cnn <- ask
      liftIO $ withTransaction cnn (\cnn -> quickQuery cnn stmt vals)
      v <- liftIO $ withTransaction cnn (\cnn -> quickQuery cnn "SELECT last_insert_rowid();" [])
      return $ concat <$> replicate (length vals) <$> v 
      where 
         stmt = printf "INSERT INTO `%s` (%s) VALUES (%s)" table column qqq
         qqq = intercalate "," $ map (const "?") vals
   unsafeUnwrap (SaveR a) = a
   
instance DB Load Value where
   optimize = map (pure *** fmap pure)
   sendSql (Load _ _ _) = True
   sendSql (LoadR _) = True
   sendSql _ = False
   sendSqlR (LoadR _) = True
   sendSqlR _ = False
   wrapCnst (LoadV a) = Value a
   wrapCnst LoadN = ValueN
   wrapVal = Value 
   wrapVals = Values
   fromSqlVal (Value x) = [("tag", toJSON "Value"), ("contents", sql2aeson x)]
   fromSqlVal ValueN = [("tag", toJSON "ValueN"), ("contents", toJSON ([] :: [()]))]
   exec (Load table column whereClause) = do
      cnn <- ask
      v <- lift $ withTransaction cnn (\cnn -> quickQuery cnn stmt [])
      return v 
      where stmt = printf "SELECT `%s` FROM `%s` WHERE %s" column table whereClause
   unsafeUnwrap (LoadR x) = x

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
   
instance (Model m, Generic (m a), GShow' (Rep (m a))) => Show (m a) where
   show = gshow

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
      b = LoadV 3,
      c = LoadN
   }
   
   load test :: [Test Value]
   
   test2 :: Test Save {
      a = Save "Table" "Column" 4,
      b = Replace "Table" "Column" 4
   }
   
@
-}

sndM :: (Monad m) => (b -> m c) -> (a,b) -> m (a,c)
sndM = runKleisli . second . Kleisli
