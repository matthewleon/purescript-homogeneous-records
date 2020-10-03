module Test.Main where

import Prelude
import Record.Homogeneous

import Effect (Effect)
import Test.Assert (assert')
import Type.Prelude (RProxy(..))

type WithoutVals type_ =
  ( a :: type_
  , b :: type_
  )

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
