{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Run where

import Test.Hspec
import GHC.Generics
import DB.Model

test :: IO ()
test = hspec $ do

   describe "Model.Internal.Run" $ do
      specify "Generics :+: L1, :*:, K1" $
         run (Test1 "sqlcmd" "sqlcmd") `shouldBeM` (Test1 1 1 :: Test Int)
      specify "Generics :+: R1, :*:, K1" $
         run (Test2 "sqlcmd" "sqlcmd") `shouldBeM` (Test2 1 1 :: Test Int)

   describe "Model.Internal.Save/Load/Delete" $ do
      specify "Convert L1, change K1" $
         save (Test1 1 1 :: Test Int) `shouldBe` (Test1 "cmd" "cmd")


   where
      shouldBeM a b = a >>= shouldBe b

data Test a = Test1 {
      f1 :: a,
      f2 :: a
   } | Test2 {
      f3 :: a,
      f4 :: a
   } deriving (Generic, Show, Eq)

instance Run Test
instance Save Test Int String where
   save v = Test1 "cmd" "cmd"

instance DB String Int where
   exec _ = return 1
