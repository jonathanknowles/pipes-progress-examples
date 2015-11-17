module Pipes.Nested where

import Control.Monad
    ( (>=>) )
import Control.Foldl
    ( Fold )
import Pipes
    ( (<-<)
    , (>->)
    , ListT ( Select )
    , Producer
    , enumerate
    , for
    , yield )

import qualified Control.Arrow as A
import qualified Control.Foldl as F
import qualified Lens.Family   as L
import qualified Pipes         as P
import qualified Pipes.Extra   as P
import qualified Pipes.Group   as P
import qualified Pipes.Prelude as P

data StreamEvent a b
    = StreamStart a
    | StreamChunk b
    | StreamEnd   a

streamChunk :: StreamEvent a b -> Maybe b
streamChunk (StreamChunk b) = Just b
streamChunk _ = Nothing

flatten :: Monad m
    => Producer (a, Producer b m r) m r
    -> Producer (StreamEvent a b) m r
flatten abs = for abs $ \(a, bs) -> do
    yield                $ StreamStart a
    for bs $ \b -> yield $ StreamChunk b
    yield                $ StreamEnd   a

nest :: Monad m
    => (a -> Producer b m ())
    -> (b -> Producer c m ())
    -> a -> Producer (b, Producer c m ()) m ()
nest aToBs bToCs a = P.map (A.second enumerate)
        <-< (enumerate . nestLists x y) a
    where
        x = Select . aToBs
        y = Select . bToCs

nestLists :: Monad m
    => (a -> ListT m b)
    -> (b -> ListT m c)
    -> a -> ListT m (b, ListT m c)
nestLists aToBs bToCs =
    aToBs >=> \b -> pure (b, bToCs b)

nestedProducersToLists :: Monad m
    => Producer (a, Producer b m ()) m ()
    -> ListT m (a, ListT m b)
nestedProducersToLists s = Select $ s >-> P.map (A.second Select)

nestedListsToProducers :: Monad m
    => ListT m (a, ListT m b)
    -> Producer (a, Producer b m ()) m ()
nestedListsToProducers s = enumerate s >-> P.map (A.second enumerate)

sameStream (StreamEnd _) (StreamStart _) = False
sameStream _ _ = True

foldStreams :: Monad m
    => Fold i j
    -> Producer (StreamEvent o i) m r
    -> Producer j m r
foldStreams f = F.purely P.folds f
    . P.maps (>-> P.mapFilter streamChunk)
    . L.view (P.groupsBy' sameStream)

