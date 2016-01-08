{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module DB.Model.Load where

import DB.Model.Internal
import GHC.Generics
import Control.Monad

class Load a where
   load :: (DB c v) => a c -> IO (a v)
   default load :: (DB c v,
                    GLoad (Rep (a c)) (Rep (a v)),
                    Generic (a v),
                    Generic (a c))
                  => (a c) -> IO (a v)
   load a = to <$> gLoad (from a)

class GLoad c v where
   gLoad :: c p -> IO (v p)


instance GLoad U1 U1 where
   gLoad _ = return U1

instance (GLoad c v) => GLoad (M1 x y c) (M1 x y v) where
   gLoad (M1 c) = M1 <$> gLoad c

instance (GLoad c v, GLoad cs vs) => GLoad (c :*: cs) (v :*: vs) where
   gLoad (c :*: cs) = gLoad c `mult` gLoad cs
         where mult = liftM2 (:*:)

instance (GLoad c v, GLoad cs vs) => GLoad (c :+: cs) (v :+: vs) where
   gLoad (L1 c) = L1 <$> gLoad c
   gLoad (R1 c) = R1 <$> gLoad c

instance (DB c v) => GLoad (K1 x c) (K1 x v) where
   gLoad (K1 c) = K1 <$> exec c []

