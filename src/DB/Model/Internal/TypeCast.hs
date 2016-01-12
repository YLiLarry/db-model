{-# LANGUAGE MultiParamTypeClasses #-}

module DB.Model.Internal.TypeCast where
import Data.Aeson
import Database.HDBC

import Data.Scientific as S (scientific, floatingOrInteger)
import Data.Text as T (pack, unpack)
import Data.ByteString.Char8 as B (pack, unpack)

sql2aeson :: SqlValue -> Value
sql2aeson (SqlString v)     = toJSON v
sql2aeson (SqlInteger v)    = toJSON v
sql2aeson (SqlByteString v) = toJSON $ B.unpack v
sql2aeson (SqlBool v)       = toJSON v
sql2aeson (SqlInt64 v)      = toJSON v
sql2aeson SqlNull           = Null
sql2aeson v                 = error $ "TypeCast.hs: " ++ show v


aeson2sql :: Value -> SqlValue
aeson2sql (String v) = toSql v
aeson2sql (Number v) =  
  case floatingOrInteger v of
      Left float -> toSql $ (float :: Double)
      Right int  -> toSql $ (int :: Integer)
aeson2sql (Bool v)   = toSql v
aeson2sql Null       = SqlNull
aeson2sql v          = error $ "TypeCast.hs: " ++ show v

