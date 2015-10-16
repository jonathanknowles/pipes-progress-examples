module Main where

import Pipes ((>->))
import Pipes.Progress
import Pipes.Progress.Examples

import qualified System.IO          as S
import qualified System.Posix.Files as S

main :: IO ()
main = do
    putStrLn "starting"
    S.hSetBuffering S.stdout S.NoBuffering
    answer <- countFileTree' (every 0.2 >-> terminalMonitor) "/public/jsk"
    answer <- countFileTree' (every 0.2 >-> terminalMonitor) "/public/jsk"
    putStrLn "finished"
    {--
    let testDirectory = "/home/jsk/scratch/test"
    hash <- hashFileTree' (every 0.5 >-> terminalMonitor) testDirectory
    Prelude.print hash
    hash <- hashFileTree testDirectory
    Prelude.print hash
    hash <- hashFileTreeSimple testDirectory
    Prelude.print hash
    hasha <- hashFileOld (every 0.5 >-> terminalMonitor) "/home/jsk/scratch/test/256MiB"
    hashb <- hashFileOld (every 0.5 >-> terminalMonitor) "/home/jsk/scratch/test/512MiB"
    hashc <- hashFileOld (every 0.5 >-> terminalMonitor) "/home/jsk/scratch/test/1GiB"
    Prelude.print hasha
    Prelude.print hashb
    Prelude.print hashc
    Prelude.print (mconcat [hasha, hashb, hashc])
    --}
