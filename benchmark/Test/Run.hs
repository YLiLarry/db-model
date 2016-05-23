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

testLoadObj :: Test Load
testLoadObj = Test {
   a = Load "Test" "id" "id <= 100000",
   b = Load "Test" "f1" "id <= 100000",
   c = LoadV 1,
   d = LoadN,
   e = LoadR $ Test {
      a = Load "Test" "id" "id <= 100000",
      b = Load "Test" "f1" "id <= 100000",
      c = LoadV 1,
      d = LoadN,
      e = LoadN
   }
}
   
testSaveObj :: Test Save
testSaveObj = Test {
   a = Save "Test" "f2" 1,
   b = Save "Test" "f1" "testSaveObj",
   c = SaveN,
   d = SaveN,
   e = SaveR $ Test {
      a = Save "Test" "f2" 1,
      b = Save "Test" "f1" "testSaveObj",
      c = SaveN,
      d = SaveN,
      e = SaveN
   }
}

test :: IO ()
test = do 
   conn <- connectSqlite3 "benchmark/benchmark.db"
   hspec $ do
      -- describe "Perpare" $ do
         -- it "Populate DB..." $ do
            -- void $ replicateM 100000 (runReaderT (run testSaveObj) conn)
      describe "Benchmark" $ do
         it "Load" $ do
            print =<< length <$> (runReaderT (run testLoadObj) conn)
            