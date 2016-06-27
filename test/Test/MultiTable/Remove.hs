{-# LANGUAGE DeriveGeneric #-}

module Test.MultiTable.Remove where

import DB.Model.Internal.Prelude
import DB.Model.MultiTable
import Database.HDBC
import Database.HDBC.Sqlite3
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Test.MultiTable.New as New

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
      describe "BASIC Remove" $ do
         it "Case 1: remove from 2 tables, no recursion." $ do 
            New.test
            let relation = BASIC {
               key = IsKey [("Test", "id")],
               f1 = IsCol "Test" "f1",
               f2 = IsCol "Test" "f2",
               c1  = IsConst "const",
               n1  = IsNull
            }
            -- check
            (Right r) <- runModelT (loadR relation $ key >. 0) conn
            deleted <- mapM (\v -> runModelT (removeR relation v) conn) r
            sequence deleted `shouldSatisfy` isRight
            (Right r) <- runModelT (loadR relation $ key >. 0) conn
            length r `shouldBe` 0
            let relation = BASIC {
               key = IsKey [("Test1", "id")],
               f1 = IsNull,
               f2 = IsNull,
               c1  = IsNull,
               n1  = IsNull
            }
            -- check
            (Right r) <- runModelT (loadR relation $ key >. 0) conn
            deleted <- mapM (\v -> runModelT (removeR relation v) conn) r
            sequence deleted `shouldSatisfy` isRight
            (Right r) <- runModelT (loadR relation $ key >. 0) conn
            length r `shouldBe` 0
         it "Case 2: remove across 2 tables, no recursion." $ do
            New.test
            let relation = BASIC {
               key = IsKey [("Test", "id"), ("Test1", "test_id")],
               f1 = IsCol "Test" "f1",
               f2 = IsCol "Test1" "f2",
               c1  = IsConst "const",
               n1  = IsNull
            }
            -- check
            (Right r) <- runModelT (loadR relation $ key >. 0) conn
            deleted <- mapM (\v -> runModelT (removeR relation v) conn) r
            sequence deleted `shouldSatisfy` isRight
            (Right r) <- runModelT (loadR relation $ key >. 0) conn
            length r `shouldBe` 0
   disconnect conn
