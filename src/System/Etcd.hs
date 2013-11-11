
module System.Etcd where

import           Control.Monad.Error
import qualified Data.ByteString      as S
import qualified Data.Text            as T
import qualified Network.HTTP.Conduit as Client
import           System.Etcd.Types

set' :: [T.Text] -> S.ByteString -> Maybe Ttl -> ErrorT Error IO Response
set' = undefined

testAndSet' :: [T.Text] -> S.ByteString -> S.ByteString -> ErrorT Error IO Response
testAndSet' = undefined

-- setOnce -- only if dne

get' :: [T.Text] -> S.ByteString -> ErrorT Error IO Response
get' = undefined

getDirectory' :: [T.Text] -> S.ByteString -> ErrorT Error IO [Response]
getDirectory' = undefined

-- getMachines -- list etcd machines
-- getLeader   -- get current leader

-- getVersion

delete' :: [T.Text] -> S.ByteString -> ErrorT Error IO Response
delete' = undefined

-- blocking core call
watch' :: [T.Text] -> S.ByteString -> Maybe Idx -> ErrorT Error IO Response
watch' = undefined
