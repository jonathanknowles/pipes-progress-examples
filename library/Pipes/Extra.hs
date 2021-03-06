{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

module Pipes.Extra where

import Control.Monad (forever, when)
import Data.Monoid ((<>))
import Pipes ((>->), Pipe, Producer, await, yield, runEffect)

import qualified Control.Foldl as F
import qualified Pipes.Prelude as P

drainProducer :: Monad m => Producer a m r -> m r
drainProducer = runEffect . (>-> P.drain)

filterMap :: Monad m => (a -> Bool) -> (a -> b) -> Pipe a b m r
filterMap p m = forever $
        await >>= \x -> when (p x) (yield $ m x)
{-# INLINE filterMap #-}

mapFilter :: Monad m => (a -> Maybe b) -> Pipe a b m r
mapFilter f = loop where
    loop = f <$> await >>= maybe loop (\x -> yield x >> loop)
{-# INLINE mapFilter #-}

mfold :: (Monad m, Monoid a) => Producer a m () -> m a
mfold = F.purely P.fold F.mconcat

mscan :: (Monad m, Monoid a) => Pipe a a m r
mscan = F.purely P.scan F.mconcat
{-# INLINE mscan #-}

mscan' :: (Monad m, Monoid a) => Pipe a a m r
mscan' = loop mempty where
    loop !a = yield a >> await >>= loop . (a <>)
{-# INLINE mscan' #-}
