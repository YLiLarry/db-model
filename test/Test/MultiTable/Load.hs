{-# LANGUAGE DeriveGeneric #-}

module Test.MultiTable.Load where

import DB.Model.MultiTable
   
import Test.Hspec
import DB.Model.Internal.Prelude
import DB.Model.MultiTable
import Database.HDBC
import Database.HDBC.Sqlite3

data Case1 m = Case1 {
   id_1 :: m Int,
   f1_1 :: m String,
   f2_1 :: m Int,
   c_1  :: m String,
   n_1  :: m String
} deriving (Generic)

instance MultiTable Case1 where
   relation = Case1 {
      id_1 = IsKey [("Test", "id")],
      f1_1 = IsCol "Test" "f1",
      f2_1 = IsCol "Test" "f2",
      c_1  = IsConst "const",
      n_1  = IsNull
   }

test :: IO ()
test = do 
   conn <- connectSqlite3 "test/test.db"
   hspec $ do
      describe "BASIC LOAD" $ do
         it "Case 1: load from single table, no recursion." $ do
            r <- runModelT (load "Test.id <=5 ") conn :: IO (Either String [Case1 Value])   
            (map id_1 <$> r) `shouldBe` (Right [Val 1,Val 2,Val 3,Val 4,Val 5])
         -- it "Test Save" $ testSave conn
   disconnect conn
