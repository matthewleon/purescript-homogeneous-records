module Test.Main where

import Prelude

import Effect (Effect)
-- | import Data.List as L
-- | import Record (equal)
import Record.Homogeneous (foldlValues, foldlValuesWithIndex)
import Test.Assert (assert')

main :: Effect Unit
main = do
  assert' "foldlValues" $
    foldlValues (+) 0 {a: 1, b: 2, c: 3} == 6

  assert' "foldlValuesWithIndex" $
    foldlValuesWithIndex (\acc key val -> acc <> key <> show val) "" {a: 1, b: 2, c: 3} == "a1b2c3"
