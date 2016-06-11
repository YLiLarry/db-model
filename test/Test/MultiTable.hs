{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.MultiTable where

import Test.MultiTable.Load as Load
import Test.MultiTable.Update as Update
import Test.MultiTable.New as New
import Test.MultiTable.Remove as Remove

test :: IO ()
test = do
   New.test
   Load.test
   Update.test
   Remove.test
   
   

-- testSave :: Connection -> IO ()   
-- testSave conn = do
   -- r <- runReaderT (run testSaveObj) conn
   -- print $ r
   
