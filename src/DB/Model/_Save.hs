{-# LANGUAGE DefaultSignatures,
             FlexibleContexts,
             ScopedTypeVariables,
             MultiParamTypeClasses,
             TypeOperators #-}

module DB.Model.Save where

import DB.Model.Internal
import DB.Model.Internal.Exception
import GHC.Generics
import Control.Monad
import Control.Exception


class Save a where
   save :: (DB c v, Show (a v), Show (a c)) => a v -> a c -> IO (a v)
   default save :: (GSave (Rep (a v))
                          (Rep (a c)),
                    DB c v,
                    Generic (a v),
                    Generic (a c),
                    Show (a v),
                    Show (a c)) =>
                    a v -> a c -> IO (a v)
   save av ac = whenError `handle` (to <$> gSave (from av) (from ac))
      where
         whenError :: PatternMatchFail -> IO e
         whenError e = do
            let e = mismatch av ac "Data and DB commands must use the same constructor."
            putStr $ displayException e
            throwIO e


class GSave v c where
   gSave :: v p -> c p -> IO (v p)



instance GSave U1 U1 where
   gSave _ _ = return U1

instance (GSave v c) => GSave (M1 x y v) (M1 x y c) where
   gSave (M1 a1) (M1 a2) = M1 <$> gSave a1 a2

instance (GSave a b, GSave as bs) => GSave (a :*: as) (b :*: bs) where
   gSave (a :*: as) (b :*: bs) = gSave a b `mult` gSave as bs
         where mult = liftM2 (:*:)

instance (GSave a b, GSave as bs) => GSave (a :+: as) (b :+: bs) where
   gSave (L1 a) (L1 b) = L1 <$> gSave a b
   gSave (R1 a) (R1 b) = R1 <$> gSave a b

instance (DB c v) => GSave (K1 x v) (K1 x c) where
   gSave (K1 v) (K1 c) = K1 <$> exec c [v]


