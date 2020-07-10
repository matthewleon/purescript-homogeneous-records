module Record.Homogeneous
  ( foldlValues
  , class FoldlValues
  , foldlValuesImpl
  , foldMapValuesL

  , foldlValuesWithIndex
  , class FoldlValuesWithIndex
  , foldlValuesWithIndexImpl
  , foldMapValuesWithIndexL
  ) where

import Prelude

import Record (get) as Record
import Type.Prelude (class IsSymbol, RLProxy(RLProxy), SProxy(SProxy), reflectSymbol)
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Prim.Row as Row
import Prim.RowList as RL

foldlValues
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValues rowList row fieldType
  => (accum -> fieldType -> accum)
  -> accum
  -> Record row
  -> accum
foldlValues = foldlValuesImpl (RLProxy :: RLProxy rowList)

foldMapValuesL
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValues rowList row fieldType
  => Monoid accum
  => (fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesL f = foldlValues (\acc x -> acc <> f x) mempty

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  )
  <= FoldlValues (rowList :: RL.RowList) (row :: # Type) fieldType
  | rowList -> row fieldType
  where
    foldlValuesImpl
      :: forall accum
       . RLProxy rowList
      -> (accum -> fieldType -> accum)
      -> accum
      -> Record row
      -> accum

instance foldlValuesCons ::
  ( FoldlValues tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) => FoldlValues (RL.Cons name fieldType tailRowList) row fieldType
  where
    foldlValuesImpl _ f accum record = foldlValuesImpl tailProxy f accum' record
      where
        tailProxy :: RLProxy tailRowList
        tailProxy = RLProxy

        value :: fieldType
        value = Record.get (SProxy :: SProxy name) record

        accum' = f accum value

instance foldlValuesNil
  :: Homogeneous row fieldType
  => FoldlValues RL.Nil row fieldType
  where
    foldlValuesImpl _ _ accum _ = accum

foldlValuesWithIndex
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValuesWithIndex rowList row fieldType
  => (accum -> String -> fieldType -> accum)
  -> accum
  -> Record row
  -> accum
foldlValuesWithIndex = foldlValuesWithIndexImpl (RLProxy :: RLProxy rowList)

foldMapValuesWithIndexL
  :: forall accum row fieldType rowList
   . RL.RowToList row rowList
  => FoldlValuesWithIndex rowList row fieldType
  => Monoid accum
  => (String -> fieldType -> accum)
  -> Record row
  -> accum
foldMapValuesWithIndexL f = foldlValuesWithIndex (\acc key x -> acc <> f key x) mempty

class
  ( Homogeneous row fieldType
  , HomogeneousRowList rowList fieldType
  )
  <= FoldlValuesWithIndex (rowList :: RL.RowList) (row :: # Type) fieldType
  | rowList -> row fieldType
  where
    foldlValuesWithIndexImpl
      :: forall accum
       . RLProxy rowList
      -> (accum -> String -> fieldType -> accum)
      -> accum
      -> Record row
      -> accum

instance foldlValuesWithIndexCons ::
  ( FoldlValuesWithIndex tailRowList row fieldType
  , Homogeneous tailRow fieldType
  , HomogeneousRowList tailRowList fieldType
  , HomogeneousRowList trash fieldType
  , IsSymbol name
  , RL.RowToList row trash
  , Row.Cons name fieldType tailRow row
  ) => FoldlValuesWithIndex (RL.Cons name fieldType tailRowList) row fieldType
  where
    foldlValuesWithIndexImpl _ f accum record = foldlValuesWithIndexImpl tailProxy f accum' record
      where
        tailProxy :: RLProxy tailRowList
        tailProxy = RLProxy

        value :: fieldType
        value = Record.get (SProxy :: SProxy name) record

        key :: String
        key = reflectSymbol (SProxy :: SProxy name)

        accum' = f accum key value

instance foldlValuesWithIndexNil
  :: Homogeneous row fieldType
  => FoldlValuesWithIndex RL.Nil row fieldType
  where
    foldlValuesWithIndexImpl _ _ accum _ = accum
