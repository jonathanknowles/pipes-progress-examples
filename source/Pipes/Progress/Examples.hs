{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Pipes.Progress.Examples where

import Control.Applicative
import Control.Concurrent       hiding (yield)
import Control.Exception               (evaluate, try, throwIO)
import Control.Monad
import Data.Function                   (on)
import Data.Monoid
import Data.Text                       (Text)
import Pipes                    hiding (every)
import Pipes.ByteString         hiding (count, find, take, takeWhile, map)
import Pipes.Prelude            hiding (findi, fromHandle, toHandle, mapM_, show)
import Pipes.Progress
import Prelude                  hiding (map, readFile, take, takeWhile, FilePath)
import System.Posix             hiding (ByteCount)
import System.Posix.ByteString         (RawFilePath)
import Text.Printf (printf)
import Text.Pretty

import qualified Control.Arrow                  as A
import qualified Control.Foldl                  as F
import qualified Crypto.Hash.SHA256             as SHA256
import qualified Data.Bits                      as Bits
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as BC
import qualified Data.List                      as L
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.IO                   as T
import qualified Data.Time.Human                as H
import qualified Data.Word                      as W
import qualified Lens.Family                    as LF
import qualified Pipes                          as P
import qualified Pipes.FileSystem               as PF
import qualified Pipes.Group                    as PG
import qualified Pipes.Prelude                  as P
import qualified Pipes.Safe                     as PS
import qualified Pipes.Safe.Prelude             as PS
import qualified System.IO                      as S
import qualified System.Posix.Files.ByteString  as S

type FilePath = RawFilePath

newtype ByteCount = ByteCount W.Word64 deriving (Enum, Eq, Integral, Num, Ord, Real, Show)
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
        [      "[",    "hashed: ", pretty fhpBytesHashed   , "]"
        , " ", "[", "remaining: ", pretty fhpBytesRemaining, "]" ]

instance Pretty FileTreeHashProgress where
    pretty FileTreeHashProgress {..} = T.concat
        [      "[", "files hashed: ", pretty fthpFilesHashed, "]"
        , " ", "[", "bytes hashed: ", pretty fthpBytesHashed, "]"
        , case fthpFileCurrent of
            Nothing -> ""
            Just fc -> T.concat [" ", "[", "current: ", T.takeEnd 32 $ pretty fc, "]" ] ]

instance Pretty FilePath where
    pretty = T.decodeUtf8

instance Pretty FileTreeCountProgress where
    pretty FileTreeCountProgress {..} = T.concat
        [      "[", "bytes: ", pretty ftcpBytesCounted, "]"
        , " ", "[", "files: ", pretty ftcpFilesCounted, "]" ]

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

instance Pretty a => Pretty (Maybe a) where
    pretty (Nothing) = T.pack "<nothing>"
    pretty (Just x) = pretty x

instance Pretty String where
    pretty = T.pack

instance Pretty W.Word64 where
        pretty = T.pack . show

getFileSize :: FilePath -> IO ByteCount
getFileSize = fmap (fromIntegral . S.fileSize) . S.getFileStatus

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

terminalMonitor :: Pretty a => Monitor a
terminalMonitor = forever $ do
    update <- await
    liftIO $
        (if isFinal update then T.putStrLn else T.putStr)
        (returnToStart <> pretty (value update))

byteCounter :: F.Fold ByteString ByteCount
byteCounter = F.Fold step 0 id where step i j = i + chunkLength j

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
    S.withFile (BC.unpack s) S.ReadMode $ \i ->
    S.withFile (BC.unpack t) S.WriteMode $ \o ->
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

hashFileOld
    :: Monitor FileHashProgress
    -> FilePath
    -> IO FileHash
hashFileOld m f =
    S.withFile (BC.unpack f) S.ReadMode $ \i ->
    getFileSize f >>= \b ->
    withMonitor (map (fmap $ fileHashProgress b) >-> m) byteCounter $ \p ->
        hashFileChunksP $ fromHandle i >-> p

data FileHashProgress = FileHashProgress
    { fhpBytesHashed    :: ByteCount
    , fhpBytesRemaining :: ByteCount }

fileHashProgress :: ByteCount -> ByteCount -> FileHashProgress
fileHashProgress limit p = FileHashProgress
    { fhpBytesHashed    =         p
    , fhpBytesRemaining = limit - p }

data StreamEvent a b
    = StreamStart a
    | StreamChunk b
    | StreamEnd   a

streamChunk :: StreamEvent a b -> Maybe b
streamChunk (StreamChunk b) = Just b
streamChunk _ = Nothing

flatten
    :: Monad m
    => Producer (a, Producer b m r) m r
    -> Producer (StreamEvent a b) m r
flatten abs = for abs $ \(a, bs) -> do
    yield                $ StreamStart a
    for bs $ \b -> yield $ StreamChunk b
    yield                $ StreamEnd   a

nest :: Monad m
     => (a -> Producer b m ())
     -> (b -> Producer c m ())
     -> a -> Producer (b, Producer c m ()) m ()
nest aToBs bToCs a = P.map (A.second P.enumerate)
        <-< (P.enumerate . nestLists x y) a
    where
        x = P.Select . aToBs
        y = P.Select . bToCs

nestLists :: Monad m
    => (a -> ListT m b)
    -> (b -> ListT m c)
    -> a -> ListT m (b, ListT m c)
nestLists aToBs bToCs =
    aToBs >=> \b -> pure (b, bToCs b)

nestedProducersToLists :: Monad m
    => Producer (a, Producer b m ()) m ()
    -> ListT m (a, ListT m b)
nestedProducersToLists s = P.Select $ s >-> P.map (A.second P.Select)

nestedListsToProducers :: Monad m
    => ListT m (a, ListT m b)
    -> Producer (a, Producer b m ()) m ()
nestedListsToProducers s = P.enumerate s >-> P.map (A.second P.enumerate)

descendantFiles :: PS.MonadSafe m => FilePath -> Producer FilePath m ()
descendantFiles path = PF.onlyFiles <-< P.enumerate (PF.descendants PF.RootToLeaf path)

readFile :: PS.MonadSafe m => FilePath -> Producer FileChunk m ()
readFile path = PS.withFile (BC.unpack path) S.ReadMode fromHandle

hashFiles :: Monad m => Producer (FilePath, Producer FileChunk m ()) m () -> m FileHash
hashFiles fs = hashFileHashesP $ P.sequence <-< P.map hashFileChunksP <-< P.map snd <-< fs

hashFileChunksP :: Monad m => Producer FileChunk m () -> m FileHash
hashFileChunksP = F.purely P.fold hashFileChunks

hashFileHashesP :: Monad m => Producer FileHash m () -> m FileHash
hashFileHashesP = F.purely P.fold hashFileHashes

hashFileChunks :: F.Fold FileChunk FileHash
hashFileChunks = F.Fold SHA256.update SHA256.init (FileHash . SHA256.finalize)

hashFileHashes :: F.Fold FileHash FileHash
hashFileHashes = F.Fold mappend mempty id

-- it should be a fold that gets passed as the "counting function". That could be passed to SCAN:

-- write a stack overflow question about how to use groups
-- assume tuples (index, filepath, filechunk)
-- assume an existing function that's capable of computing the hash of a producer of filechunks

-- ideas
-- Provide a simple interface for each part of the flow to write to.
-- Write a mapping in the outer wrapper, so when the inner flow writes outwards, it is rewritten outward but with a different type.
-- Perhaps each function could take a consumer of some kind.
-- In the end, we want a single monitor thread.

hashFilesSimple :: (PS.MonadSafe m, MonadIO m) => Pipe FilePath FileHash m ()
hashFilesSimple = P.sequence <-< P.map (PS.runSafeT . hashFileChunksP . readFile)

hashFileTreeSimple :: FilePath -> IO FileHash
hashFileTreeSimple path = PS.runSafeT $ hashFileHashesP $ hashFilesSimple <-< descendantFiles path

type FileCount = W.Word64
type FileIndex = W.Word64
type FileChunk = ByteString

-- progress: [  0 %] [   0/1024 files] [   0  B/50.0 GB] [waiting]
-- progress: [ 10 %] [ 128/1024 files] [  50 MB/50.0 GB] [hashing "../some/very/big/file"]
-- progress: [ 50 %] [ 256/1024 files] [25.0 GB/50.0 GB] [hashing "../another/very/big/file"]
-- progress: [ 90 %] [ 512/1024 files] [45.0 GB/50.0 GB] [hashing "../a/tiny/file"]
-- progress: [100 %] [1024/1024 files] [50.0 GB/50.0 GB] [hashing complete]

data FileTreeHashProgress = FileTreeHashProgress
    { fthpBytesHashed :: ByteCount
    , fthpFilesHashed :: FileCount
    , fthpFileCurrent :: Maybe FilePath }

updateFileTreeHashProgress :: FileTreeHashProgress -> FileStreamEvent -> FileTreeHashProgress
updateFileTreeHashProgress p = \case
    StreamStart f -> p { fthpFileCurrent = Just f }
    StreamChunk c -> p { fthpBytesHashed = fthpBytesHashed p + fromIntegral (B.length c) }
    StreamEnd   f -> p { fthpFileCurrent = Nothing
                       , fthpFilesHashed = fthpFilesHashed p + 1 }

initialFileTreeHashProgress :: FileTreeHashProgress
initialFileTreeHashProgress = FileTreeHashProgress
    { fthpFileCurrent = Nothing
    , fthpFilesHashed = 0
    , fthpBytesHashed = 0 }

foldFileTreeHashProgress :: F.Fold FileStreamEvent FileTreeHashProgress
foldFileTreeHashProgress = F.Fold
    updateFileTreeHashProgress initialFileTreeHashProgress id

type FileStreamEvent = StreamEvent FilePath FileChunk

sameStream (StreamEnd _) (StreamStart _) = False
sameStream _ _ = True

foldStreams :: (Eq o, Monad m)
    => F.Fold i j
    -> Producer (StreamEvent o i) m r
    -> Producer j m r
foldStreams f = F.purely PG.folds f
    . PG.maps (>-> filterMap streamChunk)
    . LF.view (PG.groupsBy' sameStream)

filterMap :: Monad m => (a -> Maybe b) -> Pipe a b m r
filterMap f = forever $
    f <$> await >>= \case
        Nothing -> pure ()
        Just b  -> yield b

streamFileTree :: PS.MonadSafe m => FilePath -> Producer FileStreamEvent m ()
streamFileTree = flatten . nest descendantFiles readFile

hashFileTree :: FilePath -> IO FileHash
hashFileTree = PS.runSafeT . hashFileStream . streamFileTree

hashFileTree'
    :: Monitor FileTreeHashProgress
    -> FilePath
    -> IO FileHash
hashFileTree' m f = PS.runSafeT $
    withMonitor m foldFileTreeHashProgress $ \p ->
        hashFileStream (streamFileTree f >-> p)

hashFileStream :: Monad m => Producer FileStreamEvent m () -> m FileHash
hashFileStream = hashFileHashesP . foldStreams hashFileChunks

countFileTree'
    :: Monitor FileTreeCountProgress
    -> FilePath
    -> IO FileTreeCountProgress
countFileTree' m f = PS.runSafeT $
    withMonitor m foldFileTreeCountProgress $ \p ->
        F.purely P.fold foldFileTreeCountProgress $
            descendantFiles f >-> P.mapM (liftIO . getFileSize) >-> p

-- We can look at passing on more info, rather than making multiple system calls for each file.
-- We should measure the number of system calls made for each file, and compare this to du -hs.
-- Make a way to avoid folding twice (in the version that monitors progress).
countFileTree
    :: FilePath
    -> IO FileTreeCountProgress
countFileTree f = PS.runSafeT $
    F.purely P.fold foldFileTreeCountProgress $
        descendantFiles f >-> P.mapM (liftIO . getFileSize)

data FileTreeCountProgress = FileTreeCountProgress
    { ftcpFilesCounted :: !FileCount
    , ftcpBytesCounted :: !ByteCount } deriving Show

foldFileTreeCountProgress :: F.Fold ByteCount FileTreeCountProgress
foldFileTreeCountProgress = F.Fold
        updateFileTreeCountProgress
        initialFileTreeCountProgress id

updateFileTreeCountProgress :: FileTreeCountProgress -> ByteCount -> FileTreeCountProgress
updateFileTreeCountProgress p c = p
    { ftcpFilesCounted = ftcpFilesCounted p + 1
    , ftcpBytesCounted = ftcpBytesCounted p + c }

initialFileTreeCountProgress :: FileTreeCountProgress
initialFileTreeCountProgress = FileTreeCountProgress
    { ftcpFilesCounted = 0
    , ftcpBytesCounted = 0 }

