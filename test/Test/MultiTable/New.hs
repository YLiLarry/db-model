{-# LANGUAGE DeriveGeneric #-}

module Test.MultiTable.New where

import DB.Model.Internal.Prelude
import DB.Model.MultiTable
import Database.HDBC
import Database.HDBC.Sqlite3
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

data BASIC m = BASIC {
   key :: m Int,
   f1 :: m Int,
   f2 :: m String,
   c1  :: m String,
   n1  :: m String
} deriving (Generic)

instance MultiTableR BASIC


test :: IO ()
test = do
   conn <- connectSqlite3 "test/test.db"
   hspec $ do
      describe "BASIC NEW" $ do
         prop "Case 1: create 1 field in 1 table, no recursion." $ do 
            rf1 <- 10000 `resize` arbitrary
            rf2 <- 50 `resize` arbitrary
            return $ ioProperty $ do
               let relation = BASIC {
                  key = IsKey [("Test", "id")],
                  f1 = IsCol "Test" "f1",
                  f2 = IsCol "Test" "f2",
                  c1  = IsConst "const",
                  n1  = IsNull
               }
               let obj = BASIC {
                        key = Null,
                        f1 = Val rf1,
                        f2 = Val rf2,
                        c1  = Val "const",
                        n1  = Null  
                     }
               (Right r) <- runModelT (newR relation obj) conn
               -- check
               (Right (c:_)) <- runModelT (loadR relation $ key =. (r#key)) conn
               return $ and [f1 c == f1 obj, f2 c == f2 obj]
         prop "Case 2: create 2 fields in 2 tables, no recursion." $ do
            rf1 <- 10000 `resize` arbitrary
            rf2 <- 50 `resize` arbitrary
            return $ ioProperty $ do
               let relation = BASIC {
                  key = IsKey [("Test", "id"), ("Test1", "test_id")],
                  f1 = IsCol "Test" "f1",
                  f2 = IsCol "Test1" "f2",
                  c1  = IsConst "const",
                  n1  = IsNull
               }
               let obj = BASIC {
                        key = Null,
                        f1 = Val rf1,
                        f2 = Val rf2,
                        c1  = Val "const",
                        n1  = Null  
                     }
               (Right r) <- runModelT (newR relation obj) conn
               -- check
               (Right (c:_)) <- runModelT (loadR relation $ key =. (r#key)) conn
               return $ and [f1 c == f1 obj, f2 c == f2 obj]
   disconnect conn
