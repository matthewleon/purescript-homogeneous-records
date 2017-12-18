module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Data.Record (equal)
import Data.Record.Homogeneous (mapValues, mapWithIndex, foldlValues, foldrValues)
import Test.Assert (ASSERT, assert')

main :: Eff (assert :: ASSERT) Unit
main = do
  assert' "mapValues" $
    mapValues (_ + 1) {a: 1, b: 2} `equal` {a: 2, b: 3}
  assert' "mapWithIndex" $
    mapWithIndex (\l v -> l <> show v) {a: 1, b: 2} `equal` {a: "a1", b: "b2"}
  assert' "foldlValues" $
    foldlValues (+) 0 {a: 1, b: 2, c: 3} == 6
  assert' "foldrValues" $
    foldrValues (+) 0 {a: 1, b: 2, c: 3} == 6
