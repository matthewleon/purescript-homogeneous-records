module Record.Homogeneous.ParSequenceRecord where

import Prelude

import Record as Record
import Type.Prelude (class IsSymbol, RLProxy(RLProxy), SProxy(SProxy), reflectSymbol)
import Prim.Row as Row
import Prim.RowList as RL
import Record.Builder (Builder)
import Record.Builder as Builder
import Control.Parallel

parSequenceRecord :: forall row row' rl parM m
   . RL.RowToList row rl
  => ParSequenceRecord rl row () row' parM m
  => Record row
  -> m (Record row')
parSequenceRecord a = sequential $ Builder.build <@> {} <$> builder
  where
    builder = parSequenceRecordImpl (RLProxy :: RLProxy rl) a

class Parallel parM m <= ParSequenceRecord rl row from to parM m
  | rl -> row from to parM m
  where
    parSequenceRecordImpl :: RLProxy rl -> Record row -> parM (Builder { | from } { | to })

instance parSequenceRecordSingle ::
  ( IsSymbol name
  , Row.Cons name (m ty) trash row
  , Parallel parM m
  , Row.Lacks name ()
  , Row.Cons name ty () to
  ) => ParSequenceRecord (RL.Cons name (m ty) RL.Nil) row () to parM m where
  parSequenceRecordImpl _ a = Builder.insert namep <$> valA
    where
      namep = SProxy :: SProxy name

      valA ::  parM ty
      valA = parallel $ Record.get namep a

else instance parSequenceRecordCons ::
  ( IsSymbol name
  , Row.Cons name (m ty) trash row
  , ParSequenceRecord tail row from from' parM m
  , Row.Lacks name from'
  , Row.Cons name ty from' to
  ) => ParSequenceRecord (RL.Cons name (m ty) tail) row from to parM m where
  parSequenceRecordImpl _ a  = fn <$> valA <*> rest
    where
      namep = SProxy :: SProxy name

      valA :: parM ty
      valA = parallel $ Record.get namep a

      tailp = RLProxy :: RLProxy tail

      rest :: parM (Builder (Record from) (Record from'))
      rest = parSequenceRecordImpl tailp a

      fn :: ty -> Builder (Record from) (Record from') -> Builder (Record from) (Record to)
      fn valA' rest' = Builder.insert namep valA' <<< rest'

instance parSequenceRecordNil :: (Parallel parM m, Applicative parM) => ParSequenceRecord RL.Nil row () () parM m where
  parSequenceRecordImpl _ _ = pure identity
