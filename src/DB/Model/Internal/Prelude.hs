module DB.Model.Internal.Prelude 
   (module X, sndM, kvp2json, json2kvp, obj2kvp, unsafeTo, groupSortOn) where
   
import Text.Printf as X (printf)
import Control.Monad as X
import Control.Monad.Trans as X
import Control.Monad.Except as X
import Control.Monad.State as X
import Control.Monad.Reader as X
import Generics.Deriving.Show as X
import Data.Typeable as X (Typeable, typeOf)
import GHC.Generics as X
import Data.Proxy as X
import Data.Aeson as X (fromJSON, toJSON, FromJSON, ToJSON, GToJSON, GFromJSON)
import Database.HDBC as X
import Control.Arrow as X
import Data.Maybe as X
import Data.Either as X
import Debug.Trace as X

import Data.List.Extra (groupSortOn)
import Data.Aeson as A
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H

sndM :: (Monad m) => (b -> m c) -> (a,b) -> m (a,c)
sndM = runKleisli . second . Kleisli

kvp2json :: (ToJSON a) => [(String, a)] -> A.Value
kvp2json = toJSON . H.fromList

json2kvp :: (FromJSON a, Typeable a) => A.Value -> [(String, a)]
json2kvp = H.toList . unsafeTo

unsafeTo :: (ToJSON a, Show a, FromJSON b, Typeable b) => a -> b
unsafeTo a = 
   case r of
      A.Error m -> error $ printf "Error when converting %s to %s:\n%s" (show a) (show $ typeOf r) m
      A.Success a -> a
   where r = fromJSON $ toJSON a

obj2kvp :: (ToJSON a, Show a, FromJSON b, Typeable b) => a -> [(String, b)]
obj2kvp = json2kvp . toJSON


