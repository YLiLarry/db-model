{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Test.MultiTable where

import Test.MultiTable.Load as Load
import Test.MultiTable.Update as Update

test :: IO ()
test = do
   Load.test
   Update.test
   

-- testSave :: Connection -> IO ()   
-- testSave conn = do
   -- r <- runReaderT (run testSaveObj) conn
   -- print $ r
   
