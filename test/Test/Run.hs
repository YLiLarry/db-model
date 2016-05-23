{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE FlexibleContexts #-}

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
   a = Load "Test" "id" "id < 6",
   b = Load "Test" "f1" "id < 6",
   c = LoadV 1,
   d = LoadN,
   e = LoadN
}
   

testLoadObj :: Test Load
testLoadObj = Test {
   a = Load "Test" "id" "id < 6",
   b = Load "Test" "f1" "id < 6",
   c = LoadV 1,
   d = LoadN,
   e = LoadR testSub
}
   
testSaveObj :: Test Save
testSaveObj = Test {
   a = Save "Test" "f2" 1,
   b = Save "Test" "f1" "testSaveObj",
   c = SaveN,
   d = SaveN,
   e = SaveN
}

test :: IO ()
test = do 
   conn <- connectSqlite3 "test/test.db"
   hspec $ do
      describe "Model.Internal" $ do
         it "Test Load" $ testLoad conn
         it "Test Save" $ testSave conn
          

testLoad :: Connection -> IO ()               
testLoad conn = do
   r <- runReaderT (run testLoadObj) conn
   print $ length r

testSave :: Connection -> IO ()   
testSave conn = do
   r <- runReaderT (run testSaveObj) conn
   print $ length r
   
