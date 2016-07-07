{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DB.Model.Internal.Where 
   (WhereBuilder(..), Where, (?<), WhereCompiled(..), WhereExp, WhereExp') where

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

type WhereExp' t = Reader (t Relation) WhereExp
type Where t = Reader (t Relation) WhereCompiled

-- | @t@ is the 'MultiTable' instance
--
--   @(t Relation -> Relation a)@ is just a field record selector function
--
--   @r@ is computed recursively and will eventually become a 'Where'
-- 
-- For example, 
--
-- @  
--    data User m = User {
--       key   :: m Integer,
--       name  :: m String,
--       email :: m String
--    }
--    
--    let user <- load (name =. "bob" &. email =. "bob@example.com" &. key !=. 0)
--    liftIO $ print $ user # key 
--    -- prints key that identifies the user
-- @
class WhereBuilder t r where
   
   infix  4  =., !=., <., <=., >=., >.
   infixr 3  &.
   infixr 2  |.

   -- | sql @<@
   (<.) :: (Show a, ToJSON a) => (t Relation -> Relation a) -> a -> r
   -- | sql @<=@
   (<=.) :: (Show a, ToJSON a) => (t Relation -> Relation a) -> a -> r
   -- | sql @>@
   (>.) :: (Show a, ToJSON a) => (t Relation -> Relation a) -> a -> r
   -- | sql @>=@
   (>=.) :: (Show a, ToJSON a) => (t Relation -> Relation a) -> a -> r
   -- | sql @=@
   (=.) :: (Show a, ToJSON a) => (t Relation -> Relation a) -> a -> r
   -- | sql @!=@
   (!=.) :: (Show a, ToJSON a) => (t Relation -> Relation a) -> a -> r
   -- | sql AND
   (&.) :: WhereExp' t -> WhereExp' t -> r
   -- | sql OR
   (|.) :: WhereExp' t -> WhereExp' t -> r
   
mkWhereExp' :: (Show a, ToJSON a) => String -> (t Relation -> Relation a) -> a -> WhereExp' t
mkWhereExp' op selector v = do
   rel <- selector <$> ask
   let (tb, col) = getTC rel
   return $ Bin op (Field tb col) (Val $ unsafeTo v)
   where
      getTC (IsKey ls) = head ls
      getTC (IsCol t c) = (t, c)
      

instance WhereBuilder t (WhereExp' t) where
   (<.) = mkWhereExp' "<"
   (<=.) = mkWhereExp' "<="
   (>.) = mkWhereExp' ">"
   (>=.) = mkWhereExp' ">="
   (=.) = mkWhereExp' "="
   (!=.) = mkWhereExp' "!="
   (&.) = liftM2 (Bin "AND")
   (|.) = liftM2 (Bin "OR")
   

instance WhereBuilder t (Where t) where
   f <. v = mapReader compileWhere $ f <. v
   f <=. v = mapReader compileWhere $ f <=. v
   f >. v = mapReader compileWhere $ f >. v
   f >=. v = mapReader compileWhere $ f >=. v
   f =. v = mapReader compileWhere $ f =. v
   f !=. v = mapReader compileWhere $ f !=. v
   f &. v = mapReader compileWhere $ f &. v
   f |. v = mapReader compileWhere $ f |. v
   

-- | Build a 'Where' clause that is a part of a prepared statement. eg.
--
-- >>> ("id = ? AND name = ?" ?< [key, name])
--
-- builds as @ ...WHERE id = /key/ && name = /name/ @
(?<) :: String -> [A.Value] -> Where t
query ?< vals = return $ WhereCompiled query vals

   
compileWhere :: WhereExp -> WhereCompiled
compileWhere (Field t c) = WhereCompiled (printf "%s.%s" t c) []
compileWhere (Val v) = WhereCompiled "?" [v]
compileWhere (Bin op r1 r2) = WhereCompiled (printf "%s %s %s" str1 op str2) (vs1 ++ vs2)
   where
      WhereCompiled str1 vs1 = compileWhere r1
      WhereCompiled str2 vs2 = compileWhere r2
