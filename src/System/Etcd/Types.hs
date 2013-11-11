{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module System.Etcd.Types where

import           Control.Applicative
import           Data.Aeson          hiding (Error)
import           Data.Aeson.TH
import qualified Data.ByteString     as S
import           Data.Int
import qualified Data.Text           as T
import           Data.Time

type Ttl = Int
type Idx = Int64

data Action = Set | Delete | TestAndSet | Get | List | Watch
  deriving ( Show, Eq )

instance ToJSON Action where
  toJSON Set        = String "SET"
  toJSON Delete     = String "DELETE"
  toJSON TestAndSet = String "TESTANDSET"
  toJSON Get        = String "GET"
  toJSON List       = String "LIST"
  toJSON Watch      = String "WATCH"

instance FromJSON Action where
  parseJSON = withText "etcd action" $ \t ->
    case T.toLower t of
      "set"        -> pure Set
      "delete"     -> pure Delete
      "testandset" -> pure TestAndSet
      "get"        -> pure Get
      "list"       -> pure List
      "watch"      -> pure Watch
      _            -> fail "expecting one of \"SET|DELETE|TESTANDSET|GET|LIST|WATCH\""

data ErrorCode = EKeyNotFound
               | EPrevValueMismatch
               | ENotAFile
               | EMaxClusterSize
               | EPostValueRequired
               | EPostPrevValueRequired
               | EPostTtlNaN
               | EPostIndexNaN
               | ERaftInternalError
               | ERaftDuringLeaderElection
               | EKeywordPrefix
               | EEtcdRecovery
               | EOther {-# UNPACK #-} !Int
               deriving ( Show, Eq )

toCode :: ErrorCode -> Int
toCode EKeyNotFound              = 100
toCode EPrevValueMismatch        = 101
toCode ENotAFile                 = 102
toCode EMaxClusterSize           = 103
toCode EPostValueRequired        = 200
toCode EPostPrevValueRequired    = 201
toCode EPostTtlNaN               = 202
toCode EPostIndexNaN             = 203
toCode ERaftInternalError        = 300
toCode ERaftDuringLeaderElection = 301
toCode EKeywordPrefix            = 400
toCode EEtcdRecovery             = 500
toCode (EOther i)                = i

fromCode :: Int -> ErrorCode
fromCode 100 = EKeyNotFound
fromCode 101 = EPrevValueMismatch
fromCode 102 = ENotAFile
fromCode 103 = EMaxClusterSize
fromCode 200 = EPostValueRequired
fromCode 201 = EPostPrevValueRequired
fromCode 202 = EPostTtlNaN
fromCode 203 = EPostIndexNaN
fromCode 300 = ERaftInternalError
fromCode 301 = ERaftDuringLeaderElection
fromCode 400 = EKeywordPrefix
fromCode 500 = EEtcdRecovery
fromCode i   = EOther i

instance FromJSON ErrorCode where
  parseJSON o = fromCode <$> parseJSON o

instance ToJSON ErrorCode where
  toJSON = toJSON . toCode

data Error =
  Error { errorCode :: !ErrorCode
        , message   :: {-# UNPACK #-} !T.Text
        , cause     :: {-# UNPACK #-} !T.Text
        }
  deriving ( Show, Eq )

deriveJSON defaultOptions {
  fieldLabelModifier = id
  } ''Error

-- {"errorCode":101,"message":"The given PrevValue is not equal to the
-- value of the key","cause":"TestAndSet: one!=two"}

-- {"errorCode":101,"message":"The given PrevValue is not equal to the
-- value of the key","cause":"TestAndSet: four!="}

-- {"errorCode":100,"message":"Key Not Found","cause":"/foo"}

data Response =
  Response { action     :: !Action
           , key        :: [T.Text]
           , value      :: !(Maybe S.ByteString)
           , prevValue  :: !(Maybe S.ByteString)
           , dir        :: !Bool -- only if true
           , newKey     :: !Bool -- only if true
           , index      :: {-# UNPACK #-} !Idx
           , expiration :: !(Maybe ZonedTime)
           , ttl        :: !(Maybe Ttl)
           }
  deriving ( Show )

instance FromJSON Response where
  parseJSON = withObject "etcd response" $ \o ->
    Response <$> o .:  "action"
             <*> (parseKey <$> o .: "key")
             <*> o .:? "value"
             <*> o .:? "prevValue"
             <*> o .:? "dir"    .!= False
             <*> o .:? "newKey" .!= False
             <*> o .:  "index"
             <*> o .:? "expiration"
             <*> o .:? "ttl"
    where
      parseKey :: T.Text -> [T.Text]
      parseKey = reverse . foldr fx [] . reverse
               . filter (not . T.null) . T.split (=='/')
      fx :: T.Text -> [T.Text] -> [T.Text]
      fx "."  path   = path
      fx ".." []     = []
      fx ".." (_:xs) = xs
      fx step path   = step:path

-- {"action":"SET","key":"/message","value":"Hello
-- world","newKey":true,"index":3}

-- {"action":"GET","key":"/message","value":"Hello world","index":3}

-- {"action":"SET","key":"/message","prevValue":"Hello
-- world","value":"Hello etcd","index":4}

-- {"action":"DELETE","key":"/message","prevValue":"Hello
-- etcd","index":5}

-- {"action":"SET","key":"/foo","value":"bar","newKey":true,
-- "expiration":"2013-07-11T20:31:12.156146039-07:00","ttl":4,"index":6}

-- {"action":"SET","key":"/foo/foo","value":"barbar","newKey":true,"index":7}

-- {"action":"SET","key":"/foo","prevValue":"one","value":"two","index":10}

-- {"action":"SET","key":"/bar","value":"four","newKey":true,"index":11}

-- [{"action":"GET","key":"/foo/foo","value":"barbar","index":10},
-- {"action":"GET","key":"/foo/foo_dir","dir":true,"index":10}]
