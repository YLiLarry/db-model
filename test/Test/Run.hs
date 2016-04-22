{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

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
   d :: m Int
}

instance Model Test
deriving instance Generic (Test m)
deriving instance Show (Test Value)
deriving instance Show (Test LastID)
deriving instance Typeable Test

testObj :: Test Load
testObj = Test {
   a = Load "Test" "id" "id < 6",
   b = Load "Test" "f1" "id < 6",
   c = Const 1,
   d = ConstNull
}
   
testSave :: Test Save
testSave = Test {
   a = Save "Test" "f2" 1,
   b = Save "Test" "f1" "testSave",
   c = Ignore,
   d = Ignore
}

test :: IO ()
test = do 
   conn <- connectSqlite3 "test/test.db"
   hspec $ do
      describe "Model.Internal" $ do
         it "Test Load" $ do
            print $ to testObj
            r <- runReaderT (run $ to testObj) conn
            print r
            print $ typeOf testObj
            print $ (from <$> r :: [Test Value])
         it "Test Save" $ do
            let (a,_) = L.partition (sendSql . snd) $ to testSave
            print $ adjust a
            r <- runReaderT (run $ to testSave) conn
            print (from <$> r :: [Test LastID])
      