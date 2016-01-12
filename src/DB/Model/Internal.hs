{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.Model.Internal where

import GHC.Generics hiding (to, from)
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Database.HDBC hiding (run)
import Text.Printf
import qualified Data.Aeson as A
import Control.Arrow
import Data.List as L
import qualified Data.Map as M
import DB.Model.Internal.Exception
import DB.Model.Internal.TypeCast
import qualified Data.Vector as V
      
class Model (a :: (* -> *) -> *)
instance (Field f) => Model (a f)

instance (Model a, Generic (a m), A.GToJSON (Rep (a m))) => A.ToJSON (a m)
instance (Model a, Generic (a m), A.GFromJSON (Rep (a m))) => A.FromJSON (a m)

class Field (a :: * -> *)
instance Field Load
instance Field Value
instance Field Save
instance Field SaveID

instance (Field f, Generic (f x), A.GToJSON (Rep (f x))) => A.ToJSON (f x)
instance (Field f, Generic (f x), A.GFromJSON (Rep (f x))) => A.FromJSON (f x)


data Load x = Load String String String
            | LVal x
            | LNull 
            deriving (Generic, Generic1, Show, Eq)

data Save x = Save String String x
            | Ignore
            deriving (Generic, Generic1, Show, Eq)

data Value x = Value x
             | Null
            deriving (Generic, Generic1, Show, Eq)
            
throughMaybe :: (a -> b) -> Value a -> Value b
throughMaybe f = fromMaybe . fmap f . toMaybe 
   
toMaybe :: Value a -> Maybe a
toMaybe (Value x) = Just x
toMaybe Null = Nothing

fromMaybe :: Maybe a -> Value a
fromMaybe Nothing = Null
fromMaybe (Just x) = Value x
   
data SaveID x = SaveID Int 
              | Ignored
            deriving (Generic, Generic1, Show, Eq)

class DB b r | b -> r, r -> b where
   exec :: (IConnection cnn) => b SqlValue -> ReaderT cnn IO [[SqlValue]]
   optimize :: [(String, b SqlValue)] -> [([String], b SqlValue)] 
   toSqlVal :: A.Value -> b SqlValue
   fromSqlVal :: r SqlValue -> A.Value
   wrapVal :: SqlValue -> r SqlValue
   wrapCnst :: b SqlValue -> r SqlValue
   sendSql :: b SqlValue -> Bool

   to :: (A.ToJSON (a b)) => a b -> [(String, b SqlValue)]
   to = map (toSqlVal `second`) . M.toList . unsafeFromJSON . A.toJSON

   from :: (A.FromJSON (a r)) => [(String, r SqlValue)] -> a r
   from = unsafeFromJSON . A.toJSON . M.fromList . map (fromSqlVal `second`)

   run :: (IConnection cnn) => [(String, b SqlValue)] -> ReaderT cnn IO [[(String, r SqlValue)]]
   run a = map g <$> f <$> mapM (runKleisli . second . Kleisli $ exec) (optimize nonconstants)
      where
         (nonconstants, constants) = map (wrapCnst `second`) `second` partition (sendSql . snd) a
         g :: [(String, SqlValue)] -> [(String, r SqlValue)]
         g a = map (wrapVal `second`) a ++ constants
         f :: [([String], [[SqlValue]])] -> [[(String, SqlValue)]]
         f v = trans [ ((,) prop) <$> vals | (prop, vals) <- v' ]
            where v' = concat [ zip prop (trans matrix) | (prop, matrix) <- v ]
   

unsafeFromJSON :: A.FromJSON a => A.Value -> a
unsafeFromJSON a = 
   case A.fromJSON a of
      A.Error m -> error $ printf "Error when converting %s:\n%s" (show a) m
      A.Success a -> a


instance DB Save SaveID where
   
   
instance DB Load Value where
   optimize = map (pure `first`)
   sendSql (Load _ _ _) = True
   sendSql _ = False
   wrapVal = Value
   wrapCnst (LVal a) = Value a
   wrapCnst LNull = Null
   exec (Load table column whereClause) = do
      cnn <- ask
      v <- lift $ withTransaction cnn (\cnn -> quickQuery cnn (printf "SELECT `%s` FROM `%s` WHERE %s" column table whereClause) [])
      return v 
   exec a = error $ printf "Error when executing Load %s." (show a)
   toSqlVal obj =
      case maybeM of
         Just v  -> v
         Nothing -> error $ printf "Expecting field type 'Load' but have %s." (show obj)
      where 
         al :: M.Map String A.Value
         al = unsafeFromJSON obj
         idx :: (A.FromJSON a) => A.Value -> Int -> a
         idx c i = unsafeFromJSON (unsafeFromJSON c V.! i)      
         maybeM = do
            tag <- M.lookup "tag" al
            contents <- M.lookup "contents" al
            case unsafeFromJSON tag of
               "Load" -> Just $ Load (contents `idx` 0) (contents `idx` 1) (contents `idx` 2)
               "LVal" -> Just $ LVal $ aeson2sql contents
               "LNull" -> Just LNull
               _ -> Nothing
   fromSqlVal (Value x) = A.toJSON $ M.fromList [("tag", A.toJSON "Value"), ("contents", sql2aeson x)]
   fromSqlVal Null = A.toJSON $ M.fromList [("tag", A.toJSON "Null"), ("contents", A.toJSON ([] :: [()]))]


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
