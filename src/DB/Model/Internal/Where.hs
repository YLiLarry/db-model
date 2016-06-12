{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DB.Model.Internal.Where 
   (WhereResult(..), Where, (?<), WhereCompiled(..)) where

import DB.Model.Internal.Prelude
import DB.Model.Internal.Class

import           Data.Aeson ()
import qualified Data.Aeson as A


data WhereExp = Field Table Column
              | Val A.Value
              | Bin String WhereExp WhereExp
              | Func String [WhereExp]
            deriving (Show)          

data WhereCompiled = WhereCompiled String [A.Value] deriving (Show)

type WhereExpR t = Reader (t Relation) WhereExp
type Where t = Reader (t Relation) WhereCompiled
type Selector t a = (t Relation -> Relation a)

class WhereResult t r where
   (<.) :: (Show a, ToJSON a) => Selector t a -> a -> r
   (<=.) :: (Show a, ToJSON a) => Selector t a -> a -> r
   (>.) :: (Show a, ToJSON a) => Selector t a -> a -> r
   (>=.) :: (Show a, ToJSON a) => Selector t a -> a -> r
   (=.) :: (Show a, ToJSON a) => Selector t a -> a -> r
   (&.) :: WhereExpR t -> WhereExpR t -> r
   (|.) :: WhereExpR t -> WhereExpR t -> r
   
mkWhereExpR :: (Show a, ToJSON a) => String -> Selector t a -> a -> WhereExpR t
mkWhereExpR op selector v = do
   rel <- selector <$> ask
   let (tb, col) = getTC rel
   return $ Bin op (Field tb col) (Val $ unsafeTo v)
   where
      getTC (IsKey ls) = head ls
      getTC (IsCol t c) = (t, c)
      

instance WhereResult t (WhereExpR t) where
   (<.) = mkWhereExpR "<"
   (<=.) = mkWhereExpR "<="
   (>.) = mkWhereExpR ">"
   (>=.) = mkWhereExpR ">="
   (=.) = mkWhereExpR "="
   (&.) = liftM2 (Bin "AND")
   (|.) = liftM2 (Bin "OR")
   

instance WhereResult t (Where t) where
   f <. v = mapReader compileWhere $ f <. v
   f <=. v = mapReader compileWhere $ f <=. v
   f >. v = mapReader compileWhere $ f >. v
   f >=. v = mapReader compileWhere $ f >=. v
   f =. v = mapReader compileWhere $ f =. v
   f &. v = mapReader compileWhere $ f &. v
   f |. v = mapReader compileWhere $ f |. v
   

(?<) :: String -> [A.Value] -> Where t
query ?< vals = return $ WhereCompiled query vals

   
compileWhere :: WhereExp -> WhereCompiled
compileWhere (Field t c) = WhereCompiled (printf "%s.%s" t c) []
compileWhere (Val v) = WhereCompiled "?" [v]
compileWhere (Bin op r1 r2) = WhereCompiled (printf "%s %s %s" str1 op str2) (vs1 ++ vs2)
   where
      WhereCompiled str1 vs1 = compileWhere r1
      WhereCompiled str2 vs2 = compileWhere r2
