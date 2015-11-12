{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pipes ((>->))
import Pipes.Progress (Monitor)
import Pipes.Progress.Examples
import Pipes.Safe (MonadSafe, runSafeT)
import Text.Pretty (Pretty)

import qualified Pipes.Prelude as P
import qualified System.IO as S

monitor :: (MonadSafe m, Pretty a) => Monitor a m
monitor = terminalMonitor 0.5

testHashFileTree :: IO ()
testHashFileTree = do
    Prelude.print =<< runSafeT (hashFileTreeP "/public/jsk/scratch/empty" monitor)
    Prelude.print =<< runSafeT (hashFileTreeP "/public/jsk/scratch/small" monitor)
    Prelude.print =<< runSafeT (hashFileTreeP "/public/jsk/scratch/large" monitor)

testHashFile :: IO ()
testHashFile = do
    Prelude.print =<< runSafeT (hashFileP "/public/jsk/scratch/small/1MiB"   monitor)
    Prelude.print =<< runSafeT (hashFileP "/public/jsk/scratch/small/4MiB"   monitor)
    Prelude.print =<< runSafeT (hashFileP "/public/jsk/scratch/small/16MiB"  monitor)
    Prelude.print =<< runSafeT (hashFileP "/public/jsk/scratch/small/64MiB"  monitor)
    Prelude.print =<< runSafeT (hashFileP "/public/jsk/scratch/large/256MiB" monitor)
    Prelude.print =<< runSafeT (hashFileP "/public/jsk/scratch/large/512MiB" monitor)
    Prelude.print =<< runSafeT (hashFileP "/public/jsk/scratch/large/1GiB" monitor)

testCalculateDiskUsage :: IO ()
testCalculateDiskUsage = do
    --count <- calculateDiskUsage' (every 0.5 >-> terminalMonitor) "/public/jsk"
    count <- calculateDiskUsage "/public/jsk"
    Prelude.print count

testCountDescendantFiles :: IO ()
testCountDescendantFiles = do
    --count <- countDescendantFiles' (every 0.5 >-> terminalMonitor) "/public/jsk"
    count <- countDescendantFiles "/public/jsk"
    Prelude.print count

testNewStyle :: IO ()
testNewStyle = do
    hash <- hashFileZ "/public/jsk/scratch/large/1GiB"
    Prelude.print hash

main :: IO ()
main = do
    S.hSetBuffering S.stdout S.NoBuffering
    --testHashFileTree
    testHashFile
