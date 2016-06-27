{-# LANGUAGE DeriveGeneric #-}

module Test.SimpleTable where

import           DB.Model.SimpleTable
import qualified DB.Model.SimpleTable as S
import           DB.Model.MultiTable (MultiTable)
import qualified DB.Model.MultiTable as M

import Test.Hspec

data Test m = Test {
   key :: m Int,
   f  :: m String,
   c  :: m String,
   n  :: m String
} deriving (Generic)

instance SimpleTable Test where
   relation = Test {
      key = S.IsKey "Test" "id",
      f  = S.IsCol "f",
      c  = S.IsConst "c",
      n  = S.IsNull
   }

test :: IO ()
test = hspec $ do
   it "Convert Table.relation to MultiTable.relation" $ do
      let expect = Test {
         key = M.IsKey [("Test", "id")],
         f   = M.IsCol "Test" "f",
         c   = M.IsConst "c",
         n   = M.IsNull
      }
      M.relation `shouldBe` expect
      
      