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
    putStrLn "hashing directory trees"
    Prelude.print =<< runSafeMonitoredEffect monitor (hashTree "/path/to/example-directory-1")
    Prelude.print =<< runSafeMonitoredEffect monitor (hashTree "/path/to/example-directory-1")

testHashFile :: IO ()
testHashFile = do
    putStrLn "Hashing files"
    Prelude.print =<< runSafeMonitoredEffect monitor (hashFile "/path/to/example-file-1")
    Prelude.print =<< runSafeMonitoredEffect monitor (hashFile "/path/to/example-file-2")

testCopyFile :: IO ()
testCopyFile = do
    putStrLn "Copying files"
    runSafeMonitoredEffect monitor (copyFile "/path/to/example-file-1" "/dev/null")
    runSafeMonitoredEffect monitor (copyFile "/path/to/example-file-2" "/dev/null")

testCopyTree :: IO ()
testCopyTree = do
    putStrLn "Copying directory trees"
    runSafeMonitoredEffect monitor (copyTree "/path/to/example-directory-1" "/path/to/example-directory-3")
    runSafeMonitoredEffect monitor (copyTree "/path/to/example-directory-2" "/path/to/example-directory-4")

testCalculateDiskUsage :: IO ()
testCalculateDiskUsage = do
    putStrLn "Calculating disk usage"
    _ <- runSafeMonitoredEffect monitor $ calculateDiskUsage "/path/to/example-directory-1"
    _ <- runSafeMonitoredEffect monitor $ calculateDiskUsage "/path/to/example-directory-2"
    return ()

main :: IO ()
main = do
    S.hSetBuffering S.stdout S.NoBuffering
    putStrLn "Please edit the file 'executable/Main.hs' to run the example code."
    --testCopyTree
    --testHashTree
    --testCalculateDiskUsage
    --testHashFile
    --testCopyFile
