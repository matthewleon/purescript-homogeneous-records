module Test.Main where

import Prelude

import Effect
import Data.List as L
import Record (equal)
import Record.Homogeneous (valuesToUnfoldable, mapValues, mapWithIndex, foldlValues, foldrValues)
import Test.Assert (assert')

main :: Effect Unit
main = do
  assert' "mapValues" $
    mapValues (_ + 1) {a: 1, b: 2} `equal` {a: 2, b: 3}
  assert' "mapWithIndex" $
    mapWithIndex (\l v -> l <> show v) {a: 1, b: 2} `equal` {a: "a1", b: "b2"}
  assert' "foldlValues" $
    foldlValues (+) 0 {a: 1, b: 2, c: 3} == 6
  assert' "foldrValues" $
    foldrValues (+) 0 {a: 1, b: 2, c: 3} == 6
  assert' "valuesToUnfoldable" $
    valuesToUnfoldable {a: 1, b: 2, c: 3} == L.fromFoldable [1, 2, 3]
