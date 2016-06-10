module Test.MultiTable.Update where

import DB.Model.Internal.Prelude
import DB.Model.MultiTable
import Database.HDBC
import Database.HDBC.Sqlite3
import Test.Hspec
import Test.MultiTable.Load

test :: IO ()
test = do
   conn <- connectSqlite3 "test/test.db"
   hspec $ do
      describe "BASIC UPDATE" $ do
         it "Case 1: save one field to a table, no recursion." $ do
            -- check
            (Right [r]) <- runModelT (load "Test.id = 5") conn
            f1_1 r `shouldBe` (Val "testSave")
            -- set 
            let x = r {f1_1 = Val "f_changed"}
            runModelT (update x) conn
            (Right [a]) <- runModelT (load "Test.id = 5") conn
            f1_1 a `shouldBe` (Val "f_changed")
            -- reset
            void $ runModelT (update r) conn
   disconnect conn
   
   