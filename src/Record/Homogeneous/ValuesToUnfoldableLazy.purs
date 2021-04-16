module Record.Homogeneous.ValuesToUnfoldableLazy where

import Prelude

import Record.Homogeneous.FoldrValuesLazy (class FoldrValuesLazy, foldrValuesLazy)
import Prim.RowList as RL
import Control.Lazy as Lazy
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable, unfoldr)

valuesToUnfoldableLazy
  :: forall r fields f v
   . RL.RowToList r fields
  => FoldrValuesLazy fields r v
  => Unfoldable f
  => Record r
  -> f v
valuesToUnfoldableLazy r = unfoldr (\(LazyTupleList f) -> f unit) lazyTupleList
  where
  lazyTupleList = foldrValuesLazy
    (\v f -> (LazyTupleList \_ -> Just (Tuple v f)))
    (LazyTupleList (\_ -> Nothing))
    r

newtype LazyTupleList v =
  LazyTupleList (Unit -> Maybe (Tuple v (LazyTupleList v)))
derive newtype instance lazyLazyTupleList :: Lazy.Lazy (LazyTupleList v)

