{-# LANGUAGE ScopedTypeVariables #-}
module Test.MultiTable.Update where

import DB.Model.Internal.Prelude
import DB.MonadModel.MultiTable
import Database.HDBC
import Database.HDBC.Sqlite3
import Test.Hspec
import Test.MultiTable.Load

test :: IO ()
test = do
   conn <- connectSqlite3 "test/test.db"
   hspec $ do
      describe "BASIC UPDATE" $ do
         it "Case 1: update one field in a table, no recursion." $ do
            (Right [[largestKey]]) <- runModelT (rawQuery "SELECT MAX(id) FROM Test" []) conn
            -- check
            (Right [r]) <- runModelT (load $ key =. (cast largestKey :: Int)) conn
            -- set 
            let x = r {f2 = Val "f_changed"}
            runModelT (update x) conn
            (Right [a]) <- runModelT (load $ key =. (cast largestKey :: Int)) conn
            f2 a `shouldBe` (Val "f_changed")
            -- reset
            void $ runModelT (update r) conn
   disconnect conn
   
   