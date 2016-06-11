{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module DB.Model.Internal (module X, TableBase) where

import DB.Model.Internal.Prelude
import DB.Model.Internal.Class as X
import DB.Model.Internal.Load as X
import DB.Model.Internal.New as X
import DB.Model.Internal.Remove as X
import DB.Model.Internal.Update as X
import DB.Model.Internal.Value as X

class (Typeable a) => TableBase (a :: (* -> *) -> *)

-- instance {-# INCOHERENT #-} (TableBase a, Generic (a m), GToJSON (Rep (a m))) => ToJSON (a m)
-- instance {-# INCOHERENT #-} (TableBase a, Generic (a m), GFromJSON (Rep (a m))) => FromJSON (a m)
-- instance {-# INCOHERENT #-} (TableBase x, Generic (x a), GShow' (Rep (x a))) => GShow (x a)
   
-- instance (GShow (m a)) => Show (m a) where
--    show = gshow


{- 
@

   data Test m = Test {
      a :: m String,
      b :: m Int,
      c :: m Int
   }

   test :: Test Load
   test = Test {
      a = LoadW "Table" "Column" "id > 5",
      b = LoadV 3,
      c = LoadN
   }
   
   load test :: [Test Value]
   
   test2 :: Test New {
      a = New "Table" "Column" 4,
      b = Replace "Table" "Column" 4
   }
   
@
-}

