module Record.Homogeneous.MapIndex where

import Prelude
import Record as Record
import Type.Prelude (class IsSymbol, RProxy(RProxy), RLProxy(RLProxy), SProxy(SProxy), reflectSymbol)
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder

mapIndex :: forall row xs a b row'
   . RL.RowToList row xs
  => MapIndex xs row a b () row'
  => (String -> b)
  -> RProxy row
  -> Record row'
mapIndex f rowProxy = Builder.build builder {}
  where
    builder = mapIndexBuilder (RLProxy :: RLProxy xs) f

class MapIndex (xs :: RL.RowList) (row :: # Type) a b (from :: # Type) (to :: # Type)
  | xs -> row a b from to where
  mapIndexBuilder :: RLProxy xs -> (String -> b) -> Builder { | from } { | to }

instance mapIndexCons ::
  ( IsSymbol name
  , Row.Cons name a trash row
  , MapIndex tail row a b from from'
  , Row.Lacks name from'
  , Row.Cons name b from' to
  ) => MapIndex (RL.Cons name a tail) row a b from to where
  mapIndexBuilder _ f =
    first <<< rest
    where
      nameP = SProxy :: SProxy name
      val = f (reflectSymbol nameP)
      rest = mapIndexBuilder (RLProxy :: RLProxy tail) f
      first = Builder.insert nameP val

instance mapIndexNil :: MapIndex RL.Nil row a b () () where
  mapIndexBuilder _ _ = identity
