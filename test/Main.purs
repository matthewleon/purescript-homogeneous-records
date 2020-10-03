module Test.Main where

import Prelude

import Effect (Effect)
import Record.Homogeneous
import Test.Assert (assert')

main :: Effect Unit
main = do
  assert' "foldlValues" $
    foldlValues (+) 0 { a: 1, b: 2, c: 3 } == 6

  assert' "foldlValuesWithIndex" $
    foldlValuesWithIndex (\acc key val -> acc <> key <> show val) "" { a: 1, b: 2, c: 3 } == "a1b2c3"

  assert' "mapValuesWithIndex" $
    mapValuesWithIndex (\key val -> key <> show val) {a: 1, b: 2, c: 3} == { a: "a1", b: "b2", c: "c3" }
