{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pipes ((>->))
import Pipes.Progress
import Pipes.Progress.Examples
import Pipes.Safe (runSafeT)

import qualified Pipes.Prelude as P
import qualified System.IO as S
{-
testHashFileTree :: IO ()
testHashFileTree = do
    hash <- hashFileTree' (every 0.5 >-> terminalMonitor) "/public/jsk/scratch"
    Prelude.print hash
-}

testHashFileTreeExperimental :: IO ()
testHashFileTreeExperimental = do
    let monitor = terminalMonitor 4.0
    --Prelude.print =<< runSafeT (hashFileTreeP "/public/jsk/scratch/small/" silentMonitor)
    --Prelude.print =<< runSafeT (hashFileTreeP "/public/jsk/scratch/small/" (terminalMonitor 1.0))
    Prelude.print =<< runSafeT (hashFileTreeP "/public/jsk/scratch/empty" monitor)
    Prelude.print =<< runSafeT (hashFileTreeP "/public/jsk/scratch/empty" monitor)
    Prelude.print =<< runSafeT (hashFileTreeP "/public/jsk/scratch/empty" monitor)
    Prelude.print =<< runSafeT (hashFileTreeP "/public/jsk/scratch" monitor)
    Prelude.print =<< runSafeT (hashFileTreeP "/public/jsk/scratch/empty" monitor)
    Prelude.print =<< runSafeT (hashFileTreeP "/public/jsk/scratch/empty" monitor)
    Prelude.print =<< runSafeT (hashFileTreeP "/public/jsk/scratch/empty" monitor)
    Prelude.print =<< runSafeT (hashFileTreeP "/public/jsk/scratch" monitor)

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
    testHashFileTreeExperimental
