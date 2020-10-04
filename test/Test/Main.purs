module Test.Main where

import Prelude
import Record.Homogeneous (foldlValues, foldlValuesWithIndex, foldrValues, foldrValuesLazy, foldrValuesWithIndex, mapIndex, mapValuesWithIndex, parSequenceRecord, valuesToUnfoldableLazy)

import Effect (Effect)
import Test.Assert (assert')
import Type.Prelude (RProxy(..))
import Record.Extra (mapRecord)
import Test.ParallelRequest as Test.ParallelRequest
import Control.Monad.Cont.Trans (ContT, runContT)
import Effect.Console (logShow)
import Data.List as List

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

  assert' "foldrValues" $
    foldrValues (+) 0 {a: 1, b: 2, c: 3} == 6

  assert' "foldrValuesLazy" $
    let
      record = {a: \_ -> 1, b: \_ -> 2, c: \_ -> 3}
      (result :: Unit -> Int) = foldrValuesLazy (\field accum -> \_ -> field unit + accum unit) (\_ -> 0) record
     in result unit == 6

  assert' "foldrValuesWithIndex" $
    foldrValuesWithIndex (\key val acc -> acc <> key <> show val) "" { a: 1, b: 2, c: 3 } == "c3b2a1"

  assert' "mapIndex" $
    mapIndex (\key -> key) (RProxy :: forall type_ . RProxy (WithoutVals type_)) == { a: "a", b: "b" }

  assert' "mapValuesWithIndex" $
    mapValuesWithIndex (\key val -> key <> show val) {a: 1, b: 2, c: 3} == { a: "a1", b: "b2", c: "c3" }

  assert' "valuesToUnfoldableLazy" $
    valuesToUnfoldableLazy {a: 1, b: 2, c: 3} == List.fromFoldable [1, 2, 3]

  let
    (parSequenceRecord__config :: Reqs String) = { a: "www.purescript.org", b: "try.purescript.org" }
    (parSequenceRecord__config' :: Reqs (ContT Unit Effect String)) = mapRecord (Test.ParallelRequest.get <<< Test.ParallelRequest.request) parSequenceRecord__config
    (parSequenceRecord__config'' :: ContT Unit Effect (Reqs String)) = parSequenceRecord parSequenceRecord__config'

  runContT parSequenceRecord__config'' \(result :: Reqs String) -> do
    assert' "parSequenceRecord" $ result ==
      { a: "<html>\r\n<head><title>301 Moved Permanently</title></head>\r\n<body>\r\n<center><h1>301 Moved Permanently</h1></center>\r\n<hr><center>nginx</center>\r\n</body>\r\n</html>\r\n"
      , b: "<html>\r\n<head><title>301 Moved Permanently</title></head>\r\n<body bgcolor=\"white\">\r\n<center><h1>301 Moved Permanently</h1></center>\r\n<hr><center>nginx/1.14.0 (Ubuntu)</center>\r\n</body>\r\n</html>\r\n"
      }
    logShow "async test parSequenceRecord finished"

  logShow "tests finished"
