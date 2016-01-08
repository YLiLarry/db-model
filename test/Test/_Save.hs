{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Save where

import Test.Hspec
import GHC.Generics
import DB.Model

test :: IO ()
test = hspec $
   describe "Model.Save" $ do
      specify "Generics :+: L1, :*:, Same structure, K1" $
         save (Test1 (2::Int) 3) (Test1 "sqlcmd" "sqlcmd") `shouldBeM` Test1 1 2
      specify "Generics :+: R1, :*:, Same structure, K1" $
         save (Test2 "ab" "abc") (Test2 "sqlcmd" "sqlcmd") `shouldBeM` Test2 "b" "bc"
      specify "Generics :+: L1, :*:, Different structures, K1" $
         save (Test1 (2::Int) 3) (Test2 "sqlcmd" "sqlcmd") `shouldThrow` anyException
   where
      shouldBeM a b = a >>= shouldBe b

data Test a = Test1 {
      f1 :: a,
      f2 :: a
   } | Test2 {
      f3 :: a,
      f4 :: a
   } deriving (Generic, Show, Eq)

instance Save Test
instance Sql String Int where
   exec _ [v] = return (v - 1)
instance Sql String String where
   exec _ [v] = return (tail v)


