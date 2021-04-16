module Test.Main where

import Prelude

import Data.List as List
import Effect (Effect)
import Effect.Console (logShow)
import Record.Homogeneous (foldlValues, foldlValuesWithIndex, foldrValues, foldrValuesLazy, foldrValuesWithIndex, valuesToUnfoldableLazy)
import Test.Assert (assert')

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

  assert' "valuesToUnfoldableLazy" $
    valuesToUnfoldableLazy {a: 1, b: 2, c: 3} == List.fromFoldable [1, 2, 3]

  logShow "tests have finished"

