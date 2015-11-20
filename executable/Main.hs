{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pipes ((>->))
import Pipes.Progress (Monitor, MonitorableEffect (..), runMonitoredEffect, runSafeMonitoredEffect)
import Pipes.Progress.Examples
import Pipes.Safe (MonadSafe, runSafeT)
import Text.Pretty

import qualified Pipes.Prelude as P
import qualified System.IO as S

monitor :: (MonadSafe m, Pretty s) => Monitor s m
monitor = terminalMonitor 0.5

testHashTree :: IO ()
testHashTree = do
    putStrLn "hashing file trees"
    Prelude.print =<< runSafeMonitoredEffect monitor (hashTree "/public/jsk/scratch/empty")
    Prelude.print =<< runSafeMonitoredEffect monitor (hashTree "/public/jsk/scratch/small")
    Prelude.print =<< runSafeMonitoredEffect monitor (hashTree "/public/jsk/scratch/small2")
    Prelude.print =<< runSafeMonitoredEffect monitor (hashTree "/public/jsk/scratch/large")

testHashFile :: IO ()
testHashFile = do
    putStrLn "Hashing files"
    Prelude.print =<< runSafeMonitoredEffect monitor (hashFile "/public/jsk/scratch/small/1MiB")
    Prelude.print =<< runSafeMonitoredEffect monitor (hashFile "/public/jsk/scratch/small/4MiB")
    Prelude.print =<< runSafeMonitoredEffect monitor (hashFile "/public/jsk/scratch/small/16MiB")
    Prelude.print =<< runSafeMonitoredEffect monitor (hashFile "/public/jsk/scratch/small/64MiB")
    Prelude.print =<< runSafeMonitoredEffect monitor (hashFile "/public/jsk/scratch/large/256MiB")
    Prelude.print =<< runSafeMonitoredEffect monitor (hashFile "/public/jsk/scratch/large/512MiB")
    Prelude.print =<< runSafeMonitoredEffect monitor (hashFile "/public/jsk/scratch/large/1GiB")

testCopyFile :: IO ()
testCopyFile = do
    putStrLn "Copying files"
    runSafeMonitoredEffect monitor (copyFile "/public/jsk/scratch/small/1MiB" "/dev/null")
    runSafeMonitoredEffect monitor (copyFile "/public/jsk/scratch/small/1MiB" "/dev/null")
    runSafeMonitoredEffect monitor (copyFile "/public/jsk/scratch/small/4MiB" "/dev/null")
    runSafeMonitoredEffect monitor (copyFile "/public/jsk/scratch/small/16MiB" "/dev/null")
    runSafeMonitoredEffect monitor (copyFile "/public/jsk/scratch/small/64MiB" "/dev/null")
    runSafeMonitoredEffect monitor (copyFile "/public/jsk/scratch/large/256MiB" "/dev/null")
    runSafeMonitoredEffect monitor (copyFile "/public/jsk/scratch/large/512MiB" "/dev/null")
    runSafeMonitoredEffect monitor (copyFile "/public/jsk/scratch/large/1GiB" "/dev/null")

testCopyTree :: IO ()
testCopyTree =
    runSafeMonitoredEffect monitor (copyTree "/public/jsk/scratch/small" "/public/jsk/scratch/small2")

testCalculateDiskUsage :: IO ()
testCalculateDiskUsage = do
    putStrLn "Calculating disk usage"
    _ <- runSafeMonitoredEffect monitor $ calculateDiskUsage "/public"
    return ()

main :: IO ()
main = do
    S.hSetBuffering S.stdout S.NoBuffering
    --testCopyTree
    --testHashTree
    --testCalculateDiskUsage
    testHashFile
    testCopyFile
    --
