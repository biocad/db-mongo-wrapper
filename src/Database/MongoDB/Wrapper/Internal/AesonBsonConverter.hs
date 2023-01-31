{-# LANGUAGE CPP #-}

module Database.MongoDB.Wrapper.Internal.AesonBsonConverter
  ( fromBson, toBson
  , fromDocument
  ) where

import qualified Data.Aeson          as A
import qualified Data.Aeson.Types    as AT

#if MIN_VERSION_aeson(2, 0, 0)
import           Data.Aeson.Key      (fromText, toText)
import qualified Data.Aeson.KeyMap   as KM
#else
import qualified Data.HashMap.Strict as M
#endif

import qualified Data.Bson           as B
import           Data.Scientific     (fromFloatDigits, toBoundedInteger,
                                      toRealFloat)
import qualified Data.Text           as T
import qualified Data.Vector         as V

import           Data.Bson           ((=:))

toBson :: A.Value -> B.Value
toBson (A.String s) = B.String s
toBson (A.Number n) =
  case toBoundedInteger n of
    Nothing -> B.Float $ toRealFloat n
    Just x  -> B.Int64 x
toBson (A.Bool b)   = B.Bool b
toBson (A.Array a)  = B.Array $ map toBson (V.toList a)
#if MIN_VERSION_aeson(2, 0, 0)
toBson (A.Object o) = B.Doc $ map (\(k, v) -> toText k =: toBson v) (KM.toList o)
#else
toBson (A.Object o) = B.Doc $ map (\(k, v) -> k =: toBson v) (M.toList o)
#endif
toBson A.Null       = B.Null

fromBson :: B.Value -> A.Value
fromBson (B.Float f)   = A.Number $ fromFloatDigits f
fromBson (B.String s)  = A.String s
fromBson (B.Doc d)     = A.object $ map fieldToPair d
fromBson (B.Array a)   = A.Array . V.fromList $ map fromBson a
fromBson (B.ObjId n)   = A.String . T.pack $ show n
fromBson (B.Bool b)    = A.Bool b
fromBson (B.UTC t)     = A.String . T.pack $ show t
fromBson (B.Int32 n)   = A.Number $ fromIntegral n
fromBson (B.Int64 n)   = A.Number $ fromIntegral n
fromBson (B.Uuid u)    = A.String . T.pack $ show u
fromBson (B.RegEx r)   = A.String . T.pack $ show r
fromBson B.Null        = A.Null
-- discard these BSON values
fromBson (B.Bin _)     = A.Null
fromBson (B.Fun _)     = A.Null
fromBson (B.Md5 _)     = A.Null
fromBson (B.UserDef _) = A.Null
fromBson (B.Stamp _)   = A.Null
fromBson (B.MinMax _)  = A.Null
fromBson (B.JavaScr _) = A.Null
fromBson (B.Sym _)     = A.Null

fieldToPair :: B.Field -> AT.Pair
#if MIN_VERSION_aeson(2, 0, 0)
fieldToPair f = (fromText (B.label f), fromBson (B.value f))
#else
fieldToPair f = (B.label f, fromBson (B.value f))
#endif

fromDocument :: B.Document -> A.Value
fromDocument d = A.object $ map fieldToPair d
