module Record.Homogeneous.FoldrValuesLazy where

import Prelude

import Record (get) as Record
import Type.Prelude (class IsSymbol, RLProxy(RLProxy), SProxy(SProxy))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Prim.Row as Row
import Prim.RowList as RL
import Control.Lazy as Lazy

foldrValuesLazy
  :: forall accum row fieldType rowList
   . Lazy.Lazy accum
  => RL.RowToList row rowList
  => FoldrValuesLazy rowList row fieldType
  => (fieldType -> accum -> accum)
  -> accum
  -> Record row
  -> accum
foldrValuesLazy = foldrValuesLazyImpl (RLProxy :: RLProxy rowList)

foldMapValuesLazyR
  :: forall accum row fieldType rowList
   . Lazy.Lazy accum
  => RL.RowToList row rowList
  => FoldrValuesLazy rowList row fieldType
  => Monoid accum
  => (fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesLazyR f = foldrValuesLazy (\x acc -> acc <> f x) mempty

class ( Homogeneous row fieldType
      , HomogeneousRowList rl fieldType
      )
   <= FoldrValuesLazy rl row fieldType
    | row -> fieldType
  where
    foldrValuesLazyImpl
      :: forall accum
       . Lazy.Lazy accum
      => RLProxy rl
      -> (fieldType -> accum -> accum)
      -> accum
      -> Record row
      -> accum

instance foldrValuesLazyCons ::
  ( FoldrValuesLazy tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) => FoldrValuesLazy (RL.Cons name fieldType tailRowList) row fieldType
  where
    foldrValuesLazyImpl _ f accum record = Lazy.defer \_ ->
      f value $ foldrValuesLazyImpl tailProxy f accum record
      where
        tailProxy :: RLProxy tailRowList
        tailProxy = RLProxy

        value :: fieldType
        value = Record.get (SProxy :: SProxy name) record

instance foldrValuesLazyNil
  :: Homogeneous row fieldType
  => FoldrValuesLazy RL.Nil row fieldType
  where
    foldrValuesLazyImpl _ _ accum _ = accum
