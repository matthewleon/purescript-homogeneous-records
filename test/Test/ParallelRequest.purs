module Test.ParallelRequest where
import Prelude

import Control.Monad.Cont.Trans (ContT(..), runContT)
import Effect (Effect)
import Effect.Console (logShow)

-- | from https://github.com/purescript/purescript-parallel/blob/master/test/Main.purs

newtype Request = Request
  { host :: String
  , path :: String
  }

foreign import getImpl :: Request -> (String -> Effect Unit) -> Effect Unit

get :: Request -> ContT Unit Effect String
get req = ContT (getImpl req)

request :: String -> Request
request host = Request { host: host, path: "/" }
