module Record.Homogeneous.MapValuesWithIndex where

import Prelude

import Record as Record
import Type.Prelude (class IsSymbol, RProxy(RProxy), RLProxy(RLProxy), SProxy(SProxy), reflectSymbol)
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder

-- Like `mapRecord` from https://github.com/justinwoo/purescript-record-extra
-- but `mapRecordWithIndex`

mapValuesWithIndex :: forall row xs a b row'
   . RL.RowToList row xs
  => MapValuesWithIndex xs row a b () row'
  => (String -> a -> b)
  -> Record row
  -> Record row'
mapValuesWithIndex f r = Builder.build builder {}
  where
    builder = mapValuesWithIndexBuilder (RLProxy :: RLProxy xs) f r

class MapValuesWithIndex (xs :: RL.RowList) (row :: # Type) a b (from :: # Type) (to :: # Type)
  | xs -> row a b from to where
  mapValuesWithIndexBuilder :: RLProxy xs -> (String -> a -> b) -> Record row -> Builder { | from } { | to }

instance mapValuesWithIndexCons ::
  ( IsSymbol name
  , Row.Cons name a trash row
  , MapValuesWithIndex tail row a b from from'
  , Row.Lacks name from'
  , Row.Cons name b from' to
  ) => MapValuesWithIndex (RL.Cons name a tail) row a b from to where
  mapValuesWithIndexBuilder _ f r =
    first <<< rest
    where
      nameP = SProxy :: SProxy name
      val = f (reflectSymbol nameP) (Record.get nameP r)
      rest = mapValuesWithIndexBuilder (RLProxy :: RLProxy tail) f r
      first = Builder.insert nameP val

instance mapValuesWithIndexNil :: MapValuesWithIndex RL.Nil row a b () () where
  mapValuesWithIndexBuilder _ _ _ = identity
