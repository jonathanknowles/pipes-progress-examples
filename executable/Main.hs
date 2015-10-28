{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pipes ((>->))
import Pipes.Progress
import Pipes.Progress.Examples

import qualified System.IO as S

testHashFileTree :: IO ()
testHashFileTree = do
    hash <- hashFileTree' (every 0.5 >-> terminalMonitor) "/public/jsk/scratch"
    --hash <- hashFileTree "/public/jsk/scratch"
    Prelude.print hash

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

main :: IO ()
main = do
    S.hSetBuffering S.stdout S.NoBuffering
    --testHashFileTree
    testCalculateDiskUsage
    --testCountDescendantFiles

