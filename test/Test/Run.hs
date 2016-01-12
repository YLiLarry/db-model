{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Run where

import Test.Hspec
import GHC.Generics hiding (from, to)
import DB.Model
import Data.Map
import qualified Data.Aeson as A
import Database.HDBC hiding (run)
import Database.HDBC.Sqlite3
import Control.Monad.Reader
import Data.Typeable

data Test m = Test {
   a :: m String,
   b :: m Int,
   c :: m Int,
   d :: m Int
}

instance Model Test
deriving instance Generic (Test m)
deriving instance Show (Test Value)
deriving instance Typeable Test

testObj :: Test Load
testObj = Test {
   a = Load "User" "name" "idx < 6",
   b = Load "User" "idx" "idx < 6",
   c = LVal 1,
   d = LNull
}
   
testJSON = A.toJSON testObj
   

test :: IO ()
test = hspec $ do
   describe "Model.Internal" $ do
      it "" $ do
         conn <- connectSqlite3 "test/test.db"
         print testJSON
         -- print $ to testObj
         r <- runReaderT (run $ to testObj) conn
         print r
         print $ typeOf testObj
         print $ (from <$> r :: [Test Value])
         -- print (toList <$> A.fromJSON testJSON :: Result [(String, A.Value)])
      