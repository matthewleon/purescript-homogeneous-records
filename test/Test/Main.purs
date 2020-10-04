module Test.Main where

import Prelude
import Record.Homogeneous

import Effect (Effect)
import Test.Assert (assert')
import Type.Prelude (RProxy(..))
import Record.Extra
import Test.ParallelRequest as Test.ParallelRequest
import Control.Monad.Cont.Trans (ContT(..), runContT)
import Effect (Effect)
import Effect.Console (logShow)

type WithoutVals type_ =
  ( a :: type_
  , b :: type_
  )

type Reqs type_ =
  { a :: type_
  , b :: type_
  }

main :: Effect Unit
main = do
  assert' "foldlValues" $
    foldlValues (+) 0 { a: 1, b: 2, c: 3 } == 6

  assert' "foldlValuesWithIndex" $
    foldlValuesWithIndex (\acc key val -> acc <> key <> show val) "" { a: 1, b: 2, c: 3 } == "a1b2c3"

  assert' "mapValuesWithIndex" $
    mapValuesWithIndex (\key val -> key <> show val) {a: 1, b: 2, c: 3} == { a: "a1", b: "b2", c: "c3" }

  assert' "mapIndex" $
    mapIndex (\key -> key) (RProxy :: forall type_ . RProxy (WithoutVals type_)) == { a: "a", b: "b" }

  let
    (reqsConfig :: Reqs String) = { a: "www.purescript.org", b: "try.purescript.org" }
    (reqsConfig' :: Reqs (ContT Unit Effect String)) = mapRecord (Test.ParallelRequest.get <<< Test.ParallelRequest.request) reqsConfig
    (reqsConfig'' :: ContT Unit Effect (Reqs String)) = parSequenceRecord reqsConfig'

  runContT reqsConfig'' \(result :: Reqs String) ->
    assert' "parSequenceRecord" $ result ==
      { a: "<html>\r\n<head><title>301 Moved Permanently</title></head>\r\n<body>\r\n<center><h1>301 Moved Permanently</h1></center>\r\n<hr><center>nginx</center>\r\n</body>\r\n</html>\r\n"
      , b: "<html>\r\n<head><title>301 Moved Permanently</title></head>\r\n<body bgcolor=\"white\">\r\n<center><h1>301 Moved Permanently</h1></center>\r\n<hr><center>nginx/1.14.0 (Ubuntu)</center>\r\n</body>\r\n</html>\r\n"
      }

