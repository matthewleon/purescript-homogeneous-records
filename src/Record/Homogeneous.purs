module Record.Homogeneous
  ( valuesToUnfoldable

  , mapValues
  , class MapValues
  , mapValuesImpl

  , mapWithIndex
  , class MapWithIndex
  , mapWithIndexImpl

  , foldlValues
  , class FoldlValues
  , foldlValuesImpl

  , foldrValues
  , class FoldrValues
  , foldrValuesImpl

  , foldrValuesLazy
  , class FoldrValuesLazy
  , foldrValuesLazyImpl
  ) where

import Prelude

import Control.Lazy as Z
import Data.Maybe (Maybe(..))
import Record (get)
import Record.Builder (Builder, build, insert)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)
import Type.Row (class Lacks)
import Type.RowList (class RowToList, RLProxy(..))
import Type.Row.Homogeneous (class Homogeneous, class HomogeneousRowList)
import Prim.RowList

valuesToUnfoldable
  :: forall r fields f v
   . RowToList r fields
  => FoldrValuesLazy fields r v
  => Unfoldable f
  => Record r
  -> f v
valuesToUnfoldable r = unfoldr (\(LazyTupleList f) -> f unit) lazyTupleList
  where
  lazyTupleList = foldrValuesLazy
    (\v f -> (LazyTupleList \_ -> Just (Tuple v f)))
    (LazyTupleList (\_ -> Nothing))
    r

newtype LazyTupleList v =
  LazyTupleList (Unit -> Maybe (Tuple v (LazyTupleList v)))
derive newtype instance lazyLazyTupleList :: Z.Lazy (LazyTupleList v)

mapValues
  :: forall r t r' t' fields
   . RowToList r fields
  => MapValues fields r r' t t'
  => (t -> t')
  -> Record r
  -> Record r'
mapValues f r = build (mapValuesImpl (RLProxy :: RLProxy fields) f r) {}

class ( Homogeneous row fieldType
      , HomogeneousRowList rl fieldType
      , Homogeneous row' fieldType'
      )
   <= MapValues rl row row' fieldType fieldType'
    | rl -> row'
    , row' -> fieldType'
    , row -> fieldType
  where
    mapValuesImpl
      :: RLProxy rl
      -> (fieldType -> fieldType')
      -> Record row
      -> Builder {} (Record row')

instance mapValuesCons ::
  ( MapValues tail row tailRow' fieldType fieldType'
  , Homogeneous row fieldType
  , Homogeneous row' fieldType'
  , IsSymbol name
  , RowCons name fieldType tailRow row
  , Lacks name tailRow'
  , RowCons name fieldType' tailRow' row'
  ) => MapValues (Cons name fieldType tail) row row' fieldType fieldType'
  where
    mapValuesImpl _ f record = insert nameP value <<< rest
      where
        nameP = SProxy :: SProxy name
        value = f (get nameP record)
        rest = mapValuesImpl (RLProxy :: RLProxy tail) f record

instance mapValuesNil
  :: Homogeneous row fieldType
  => MapValues Nil row () fieldType fieldType'
  where
    mapValuesImpl _ _ _ = id

mapWithIndex
  :: forall r t r' t' fields
   . RowToList r fields
  => MapWithIndex fields r r' t t'
  => (String -> t -> t')
  -> Record r
  -> Record r'
mapWithIndex f r = build (mapWithIndexImpl (RLProxy :: RLProxy fields) f r) {}

class ( Homogeneous row fieldType
      , HomogeneousRowList rl fieldType
      , Homogeneous row' fieldType'
      )
   <= MapWithIndex rl row row' fieldType fieldType'
    | rl -> row'
    , row' -> fieldType'
    , row -> fieldType
  where
    mapWithIndexImpl
      :: RLProxy rl
      -> (String -> fieldType -> fieldType')
      -> Record row
      -> Builder {} (Record row')

instance mapWithIndexCons ::
  ( MapWithIndex tail row tailRow' fieldType fieldType'
  , Homogeneous row fieldType
  , Homogeneous row' fieldType'
  , IsSymbol name
  , RowCons name fieldType tailRow row
  , RowLacks name tailRow'
  , RowCons name fieldType' tailRow' row'
  ) => MapWithIndex (Cons name fieldType tail) row row' fieldType fieldType'
  where
    mapWithIndexImpl _ f record = insert nameP value <<< rest
      where
        nameP = SProxy :: SProxy name
        rest = mapWithIndexImpl (RLProxy :: RLProxy tail) f record
        value = f (reflectSymbol nameP) (get nameP record)

instance mapWithIndexNil
  :: Homogeneous row fieldType
  => MapWithIndex Nil row () fieldType fieldType'
  where
    mapWithIndexImpl _ _ _ = id

foldlValues
  :: forall b r t fields
   . RowToList r fields
  => FoldlValues fields r t
  => (b -> t -> b)
  -> b
  -> Record r
  -> b
foldlValues = foldlValuesImpl (RLProxy :: RLProxy fields)

class ( Homogeneous row fieldType
      , HomogeneousRowList rl fieldType
      )
   <= FoldlValues rl row fieldType
    | row -> fieldType
  where
    foldlValuesImpl
      :: forall b
       . RLProxy rl
      -> (b -> fieldType -> b)
      -> b
      -> Record row
      -> b

instance foldlValuesCons ::
  ( FoldlValues tail row fieldType
  , Homogeneous row fieldType
  , IsSymbol name
  , RowCons name fieldType tailRow row
  ) => FoldlValues (Cons name fieldType tail) row fieldType
  where
    foldlValuesImpl _ f acc record = foldlValuesImpl tailProxy f acc' record
        where
          tailProxy = (RLProxy :: RLProxy tail)
          value = get (SProxy :: SProxy name) record
          acc' = f acc value

instance foldlValuesNil
  :: Homogeneous row fieldType
  => FoldlValues Nil row fieldType
  where
    foldlValuesImpl _ _ acc _ = acc

-- | Not stack safe, but realistically this shouldn't matter on records.
foldrValues
  :: forall b r t fields
   . RowToList r fields
  => FoldrValues fields r t
  => (t -> b -> b)
  -> b
  -> Record r
  -> b
foldrValues = foldrValuesImpl (RLProxy :: RLProxy fields)

class ( Homogeneous row fieldType
      , HomogeneousRowList rl fieldType
      )
   <= FoldrValues rl row fieldType
    | row -> fieldType
  where
    foldrValuesImpl
      :: forall b
       . RLProxy rl
      -> (fieldType -> b -> b)
      -> b
      -> Record row
      -> b

instance foldrValuesCons ::
  ( FoldrValues tail row fieldType
  , Homogeneous row fieldType
  , IsSymbol name
  , RowCons name fieldType tailRow row
  ) => FoldrValues (Cons name fieldType tail) row fieldType
  where
    foldrValuesImpl _ f b rec = f value (foldrValuesImpl tailProxy f b rec)
        where
          tailProxy = (RLProxy :: RLProxy tail)
          value = get (SProxy :: SProxy name) rec

instance foldrValuesNil
  :: Homogeneous row fieldType
  => FoldrValues Nil row fieldType
  where
    foldrValuesImpl _ _ b _ = b

foldrValuesLazy
  :: forall b r t fields
   . Z.Lazy b
  => RowToList r fields
  => FoldrValuesLazy fields r t
  => (t -> b -> b)
  -> b
  -> Record r
  -> b
foldrValuesLazy = foldrValuesLazyImpl (RLProxy :: RLProxy fields)

class ( Homogeneous row fieldType
      , HomogeneousRowList rl fieldType
      )
   <= FoldrValuesLazy rl row fieldType
    | row -> fieldType
  where
    foldrValuesLazyImpl
      :: forall b
       . Z.Lazy b
      => RLProxy rl
      -> (fieldType -> b -> b)
      -> b
      -> Record row
      -> b

instance foldrValuesLazyCons ::
  ( FoldrValuesLazy tail row fieldType
  , Homogeneous row fieldType
  , IsSymbol name
  , RowCons name fieldType tailRow row
  ) => FoldrValuesLazy (Cons name fieldType tail) row fieldType
  where
    foldrValuesLazyImpl _ f b rec = Z.defer \_ ->
      f value (foldrValuesLazyImpl tailProxy f b rec)
      where
        tailProxy = (RLProxy :: RLProxy tail)
        value = get (SProxy :: SProxy name) rec

instance foldrValuesLazyNil
  :: Homogeneous row fieldType
  => FoldrValuesLazy Nil row fieldType
  where
    foldrValuesLazyImpl _ _ b _ = b
