{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pipes ((>->))
import Pipes.Progress
import Pipes.Progress.Examples

import qualified System.IO          as S
import qualified System.Posix.Files as S

testHashFileTree :: IO ()
testHashFileTree = do
    hash <- hashFileTree' (every 0.5 >-> terminalMonitor) "/public/jsk/scratch"
    Prelude.print hash

testCountFileTree :: IO ()
testCountFileTree = do
    count <- countFileTree' (every 0.5 >-> terminalMonitor) "/public/jsk"
    Prelude.print count

main :: IO ()
main = do
    S.hSetBuffering S.stdout S.NoBuffering
    testHashFileTree
    testCountFileTree

