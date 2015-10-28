{-# LANGUAGE LambdaCase #-}

module Pipes.Extra
    ( filterMap )
    where

import Control.Monad
    ( forever )
import Pipes
    ( Pipe
    , await
    , yield )

filterMap :: Monad m => (a -> Maybe b) -> Pipe a b m r
filterMap f = forever $
    f <$> await >>= \case
        Nothing -> pure ()
        Just b  -> yield b

