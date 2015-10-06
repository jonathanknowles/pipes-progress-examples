{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}

module Pipes.Progress.Examples where

import Control.Applicative
import Control.Concurrent       hiding (yield)
import Control.Exception               (evaluate, try, throwIO)
import Control.Monad
import Data.Monoid
import Data.Text                       (Text)
import Pipes                    hiding (every)
import Pipes.ByteString         hiding (count, find, take, takeWhile, map)
import Pipes.Prelude            hiding (findi, fromHandle, toHandle, mapM_, show)
import Pipes.Progress
import Prelude                  hiding (map, readFile, take, takeWhile)
import System.Posix             hiding (ByteCount)
import Text.Printf (printf)
import Text.Pretty

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.Bits          as Bits
import qualified Data.ByteString    as B
import qualified Data.List          as L
import qualified Data.Text          as T
import qualified Data.Text.IO       as T
import qualified Data.Time.Human    as H
import qualified Data.Word          as W
import qualified Pipes              as P
import qualified Pipes.FileSystem   as PF
import qualified Pipes.Prelude      as P
import qualified Pipes.Safe         as PS
import qualified Pipes.Safe.Prelude as PS
import qualified System.IO          as S
import qualified GHC.IO.Exception   as G

newtype ByteCount = ByteCount Integer deriving (Enum, Eq, Integral, Num, Ord, Real, Show)
newtype TimeElapsed = TimeElapsed Period deriving (Enum, Eq, Fractional, Num, Ord, Real, RealFrac, Show)
newtype TimeRemaining = TimeRemaining Period deriving (Enum, Eq, Fractional, Num, Ord, Real, RealFrac, Show)
newtype TransferRate = TransferRate Double deriving (Enum, Eq, Fractional, Num, Ord, Real, RealFrac, Show)

transferRate :: ByteCount -> Period -> TransferRate
transferRate (ByteCount b) (Period t) = TransferRate $ fromIntegral b / realToFrac t

initProgress :: ByteCount -> RichFileCopyProgress
initProgress bytesTarget = RichFileCopyProgress
    { rfcpBytesCopied   = 0
    , rfcpBytesTarget   = bytesTarget
    , rfcpTimeElapsed   = 0
    , rfcpTimeRemaining = 0
    , rfcpTransferRate  = 0
    }

data Percent a = Percent a a

instance (Real a, Show a) => Pretty (Percent a) where
    pretty (Percent a b) = T.pack
        (show $ floor
            ((100.0 * realToFrac a)
                    / realToFrac b)) <> "%"

instance Pretty Integer where
    pretty a = if a < 0
               then "-" <> pretty (-a)
               else T.reverse
                  $ T.intercalate ","
                  $ T.chunksOf 3
                  $ T.reverse
                  $ T.pack
                  $ show a

instance Pretty (ByteCount, ByteCount) where
    pretty (ByteCount a, ByteCount b) =
        pretty (fromIntegral a :: Integer) <> " / " <>
        pretty (fromIntegral b :: Integer) <> " bytes"

instance Pretty ByteCount where
    pretty (ByteCount a) =
        pretty a <> " " <>
            if a == 1 || a == -1
                then "byte"
                else "bytes"

instance Pretty TransferRate where
    pretty (TransferRate a) =
        pretty (floor a :: Integer) <> " bytes/s"

instance Pretty TimeElapsed where
    pretty a = pretty $ H.Human 2 $ H.TimePeriod (floor a) H.Second

instance Pretty TimeRemaining where
    pretty a = pretty $ H.Human 2 $ H.TimePeriod (ceiling a) H.Second

instance Pretty FileCopyProgress where
    pretty FileCopyProgress {..} = T.concat
        [      "[",   "copied: ", pretty fcpBytesCopied   , "]"
        , " ", "[","remaining: ", pretty fcpBytesRemaining, "]" ]

instance Pretty FileHashProgress where
    pretty FileHashProgress {..} = T.concat
        [      "[",   "hashed: ", pretty fhpBytesHashed   , "]"
        , " ", "[","remaining: ", pretty fhpBytesRemaining, "]" ]

instance Pretty RichFileCopyProgress where
    pretty p = T.concat
        [      "[",               percentage, "]"
        , " ", "[",               bytes     , "]"
        , " ", "[",     "rate: ", rate      , "]"
        , " ", "[",  "elapsed: ", elapsed   , "]"
        , " ", "[","remaining: ", remaining , "]" ]
        where
            percentage  = pretty (Percent (rfcpBytesCopied p) (rfcpBytesTarget p))
            bytes       = pretty (rfcpBytesCopied p, rfcpBytesTarget p)
            rate        = pretty (rfcpTransferRate  p)
            elapsed     = pretty (TimeElapsed   (rfcpTimeElapsed   p))
            remaining   = pretty (TimeRemaining (rfcpTimeRemaining p))

getFileSize :: String -> IO ByteCount
getFileSize path = do
    stat <- getFileStatus path
    return (ByteCount $ fromIntegral $ fileSize stat)

progressComplete :: RichFileCopyProgress -> Bool
progressComplete p = rfcpBytesCopied p == rfcpBytesTarget p

returnToStart :: Text
returnToStart = "\r\ESC[K"

data RichFileCopyProgress = RichFileCopyProgress
    { rfcpBytesCopied   :: ByteCount
    , rfcpBytesTarget   :: ByteCount
    , rfcpTimeElapsed   :: Period
    , rfcpTimeRemaining :: Period   -- this should be Maybe
    , rfcpTransferRate  :: TransferRate -- this should be Maybe
    }

updateProgress :: RichFileCopyProgress -> Period -> ByteCount -> RichFileCopyProgress
updateProgress p t b = p
    { rfcpBytesCopied   = nBytesCopied
    , rfcpTimeElapsed   = nTimeElapsed
    , rfcpTimeRemaining = nTimeRemaining
    , rfcpTransferRate  = nTransferRate
    } where
        nBytesCopied   = b
        nTimeElapsed   = rfcpTimeElapsed p + t
        nTimeRemaining = realToFrac $ fromIntegral (rfcpBytesTarget p - b) / nTransferRate
        nTransferRate  = 0.9 * rfcpTransferRate p
                       + 0.1 * transferRate (b - rfcpBytesCopied p) t

main :: IO ()
main = do
    putStrLn "starting"
    S.hSetBuffering S.stdout S.NoBuffering
    hash <- hashFile (every 0.5 >-> terminalMonitor) "/home/jsk/scratch/test/256MiB"
    copyFile (every 0.5 >-> terminalMonitor) "/home/jsk/scratch/test/1GiB" "/dev/null"
    hash <- hashFile (every 0.5 >-> terminalMonitor) "/home/jsk/scratch/test/1GiB"
    Prelude.print hash
    --copyFile (every 0.1 >-> terminalMonitor) "/mnt/testdisk/4GiB" "/mnt/testdisk/target"
    --copyFile (every 0.1 >-> terminalMonitor) "/mnt/testdisk/4GiB" "/dev/null"
    --copyFile (every 0.1 >-> terminalMonitor) "/mnt/testdisk/1GiB" "/dev/null"

terminalMonitor :: Pretty a => Monitor a
terminalMonitor = forever $ do
    update <- await
    liftIO $
        (if isFinal update then T.putStrLn else T.putStr)
        (returnToStart <> pretty (value update))

byteCounter :: Monad m => Counter ByteString ByteCount m
byteCounter = Counter 0 $ map chunkLength >-> accumulate 0

transferData
    :: Monitor DataTransferProgress
    -> S.Handle
    -> S.Handle
    -> IO ()
transferData m i o =
    withMonitor (map (fmap dataTransferProgress) >-> m) byteCounter $ \p ->
        runEffect $ fromHandle i >-> p >-> toHandle o

chunkLength :: ByteString -> ByteCount
chunkLength = ByteCount . fromIntegral . B.length

data DataTransferProgress = DataTransferProgress
    { dtpBytesTransferred :: ByteCount }
    deriving Show

dataTransferProgress = DataTransferProgress

copyFile
    :: Monitor FileCopyProgress
    -> FilePath
    -> FilePath
    -> IO ()
copyFile m s t =
    S.withFile s S.ReadMode $ \i ->
    S.withFile t S.WriteMode $ \o ->
    getFileSize s >>= \b ->
        transferData (map (fmap $ fileCopyProgress b) >-> m) i o

data FileCopyProgress = FileCopyProgress
    { fcpBytesCopied    :: ByteCount
    , fcpBytesRemaining :: ByteCount }
    deriving Show

fileCopyProgress :: ByteCount -> DataTransferProgress -> FileCopyProgress
fileCopyProgress limit p = FileCopyProgress
    { fcpBytesCopied    =         dtpBytesTransferred p
    , fcpBytesRemaining = limit - dtpBytesTransferred p }

data FileHash = FileHash !ByteString

instance Monoid FileHash where
    mempty = FileHash $ B.replicate 32 0
    mappend (FileHash a) (FileHash b) = FileHash $ B.pack $ B.zipWith Bits.xor a b

instance Show FileHash where
    show (FileHash a) = Prelude.concatMap showHex $ B.unpack a

showHex :: W.Word8 -> String
showHex = printf "%02x"

hashFile
    :: Monitor FileHashProgress
    -> FilePath
    -> IO FileHash
hashFile m f = --fmap FileHash $
    S.withFile f S.ReadMode $ \i ->
    getFileSize f >>= \b ->
    withMonitor (map (fmap $ fileHashProgress b) >-> m) byteCounter $ \p ->
        sha256 $ fromHandle i >-> p

data FileHashProgress = FileHashProgress
    { fhpBytesHashed    :: ByteCount
    , fhpBytesRemaining :: ByteCount }

fileHashProgress :: ByteCount -> ByteCount -> FileHashProgress
fileHashProgress limit p = FileHashProgress
    { fhpBytesHashed    =         p
    , fhpBytesRemaining = limit - p }

type FileCount = Integer
type FileChunk = ByteString

integers :: Monad m => Integer -> P.ListT m Integer
integers from = P.Select $ loop from where
    loop from = yield from >> loop (from + 1)

readFile :: PS.MonadSafe m => FilePath -> P.ListT m FileChunk
readFile path = P.Select $ PS.withFile path S.ReadMode fromHandle

readFiles :: PS.MonadSafe m => P.ListT m FilePath -> P.ListT m (FilePath, P.ListT m FileChunk)
readFiles paths = paths >>= \path -> pure (path, readFile path)

hashFiles' :: Monad m => P.ListT m (FilePath, P.ListT m FileChunk) -> P.ListT m (FilePath, FileHash)
hashFiles' stream = do
    (path, chunks) <- stream
    hash <- lift $ sha256 $ P.enumerate chunks
    pure (path, hash)

hashFiles'' :: Monad m => P.ListT m (FilePath, FileHash) -> m FileHash
hashFiles'' stream = P.fold mappend mempty id (P.enumerate stream >-> P.map snd)

hashFiles :: Monad m => P.ListT m (FilePath, P.ListT m FileChunk) -> m FileHash
hashFiles = hashFiles'' . hashFiles'

hashFileTree :: PS.MonadSafe m => FilePath -> m FileHash
hashFileTree = hashFiles . readFiles . files where
    files path = P.Select $
        P.enumerate (PF.descendants PF.RootToLeaf path)
            >-> PF.onlyFiles

zipListT :: Monad m => P.ListT m a -> P.ListT m b -> P.ListT m (a, b)
zipListT xs ys = P.Select $ P.zip (P.enumerate xs) (P.enumerate ys)

data FileTreeHashProgress = FileTreeHashProgress
    { fthpBytesHashed :: ByteCount
    , fthpFilesHashed :: Integer
    , fthpFileCurrent :: Maybe FilePath }

sha256 :: Monad m => Producer FileChunk m () -> m FileHash
sha256 = fmap FileHash . fold SHA256.update SHA256.init SHA256.finalize

