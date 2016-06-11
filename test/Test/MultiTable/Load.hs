{-# LANGUAGE DeriveGeneric #-}

module Test.MultiTable.Load where

import DB.Model.MultiTable
   
import Test.Hspec
import DB.Model.Internal.Prelude
import DB.Model.MultiTable
import Database.HDBC
import Database.HDBC.Sqlite3

data BASIC m = BASIC {
   key :: m Int,
   f1 :: m Integer,
   f2 :: m String,
   c1  :: m String,
   n1  :: m String
} deriving (Generic)

instance MultiTable BASIC where
   relation = BASIC {
      key = IsKey [("Test", "id")],
      f1 = IsCol "Test" "f1",
      f2 = IsCol "Test" "f2",
      c1  = IsConst "const",
      n1  = IsNull
   }

test :: IO ()
test = do 
   conn <- connectSqlite3 "test/test.db"
   hspec $ do
      describe "BASIC LOAD" $ do
         it "Case 1: load from single table, no recursion." $ do
            (Right r) <- runModelT (load "id > 0") conn :: IO (Either String [BASIC Value])   
            length r `shouldSatisfy` (>= 100)
         -- it "Test Save" $ testSave conn
   disconnect conn
