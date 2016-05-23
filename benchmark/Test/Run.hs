{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.Run where

import Test.Hspec
import GHC.Generics hiding (from, to)
import DB.Model
import Data.Map as M hiding (adjust)
import Data.List as L hiding (adjust)
import qualified Data.Aeson as A
import Database.HDBC hiding (run)
import Database.HDBC.Sqlite3
import Control.Monad.Reader
import Data.Typeable

data Test m = Test {
   a :: m Int,
   b :: m String,
   c :: m Int,
   d :: m Int,
   e :: m (Test m)
} deriving (Generic)
instance Model Test 


testSub :: Test Load
testSub = Test {
   a = Load "Test" "id" "id = 1000",
   b = Load "Test" "f1" "id = 1000",
   c = Const 1,
   d = ConstNull,
   e = ConstNull
}
   

testLoadObj :: Test Load
testLoadObj = Test {
   a = Load "Test" "id" "id < 1000",
   b = Load "Test" "f1" "id < 1000",
   c = Const 1,
   d = ConstNull,
   e = LoadR testSub
}
   
testSaveObj :: Test Save
testSaveObj = Test {
   a = Save "Test" "f2" 1,
   b = Save "Test" "f1" "testSaveObj",
   c = Ignore,
   d = Ignore,
   e = Ignore
}

test :: IO ()
test = do 
   conn <- connectSqlite3 "benchmark/benchmark.db"
   hspec $ do
      describe "Benchmark" $ do
         it "Load" $ do
            print =<< length <$> (runReaderT (run testLoadObj) conn)
            