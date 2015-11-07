{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Pipes.Progress.Examples where

import Control.Foldl                   (Fold)
import Control.Monad                   (forever)
import Control.Monad.Trans             (MonadIO, liftIO)
import Crypto.Hash.SHA256.Extra        (SHA256, foldChunks, foldHashes)
import Data.ByteString                 (ByteString)
import Data.Monoid                     ((<>))
import Data.Text                       (Text)
import Pipes                           ((>->), (<-<), Consumer, Pipe, Producer, Proxy, await, for, runEffect, yield)
import Pipes.Core                      ((<\\), respond)
import Pipes.FileSystem                (isFile, FileInfo)
import Pipes.Nested                    (StreamEvent (..))
import Pipes.Progress                  (Monitor, Period (..))
import Pipes.Safe                      (MonadSafe, SafeT)
import Prelude                  hiding (FilePath, readFile)
import System.IO                       (IOMode, Handle)
import System.Posix.ByteString         (RawFilePath)
import Text.Printf                     (printf)
import Text.Pretty                     (Pretty, pretty)

import qualified Control.Foldl                  as F
import qualified Crypto.Hash.SHA256             as SHA256
import qualified Crypto.Hash.SHA256.Extra       as SHA256
import qualified Data.ByteString                as B
import qualified Data.ByteString.Char8          as BC
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.IO                   as T
import qualified Data.Time.Human                as H
import qualified Data.Word                      as W
import qualified Pipes                          as P
import qualified Pipes.ByteString               as PB
import qualified Pipes.FileSystem               as PF
import qualified Pipes.Nested                   as PN
import qualified Pipes.Prelude                  as P
import qualified Pipes.Progress                 as PP
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
    , rfcpTransferRate  = 0 }

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

instance Pretty FileByteCount where
    pretty FileByteCount {..} = T.concat
        [      "[", "bytes: ", pretty byteCount, "]"
        , " ", "[", "files: ", pretty fileCount, "]" ]

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
        (if PP.isFinal update then T.putStrLn else T.putStr)
        (returnToStart <> pretty (PP.value update))

returnToStart :: Text
returnToStart = "\r\ESC[K"

byteCounter :: F.Fold ByteString ByteCount
byteCounter = F.Fold step 0 id where step i j = i + chunkLength j

transferData
    :: Monitor DataTransferProgress
    -> S.Handle
    -> S.Handle
    -> IO ()
transferData m i o =
    PP.withMonitor (P.map (fmap dataTransferProgress) >-> m) 0 (F.purely P.scan byteCounter) $ \p ->
        runEffect $ PB.fromHandle i >-> p >-> PB.toHandle o

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
        transferData (P.map (fmap $ fileCopyProgress b) >-> m) i o

data FileCopyProgress = FileCopyProgress
    { fcpBytesCopied    :: ByteCount
    , fcpBytesRemaining :: ByteCount }
    deriving Show

fileCopyProgress :: ByteCount -> DataTransferProgress -> FileCopyProgress
fileCopyProgress limit p = FileCopyProgress
    { fcpBytesCopied    =         dtpBytesTransferred p
    , fcpBytesRemaining = limit - dtpBytesTransferred p }

hashFileOld
    :: Monitor FileHashProgress
    -> FilePath
    -> IO FileHash
hashFileOld m f =
    S.withFile (BC.unpack f) S.ReadMode $ \i ->
    getFileSize f >>= \b ->
    PP.withMonitor (P.map (fmap $ fileHashProgress b) >-> m) 0 (F.purely P.scan byteCounter) $ \p ->
        hashFileChunksP $ PB.fromHandle i >-> p

data FileHashProgress = FileHashProgress
    { fhpBytesHashed    :: ByteCount
    , fhpBytesRemaining :: ByteCount }

fileHashProgress :: ByteCount -> ByteCount -> FileHashProgress
fileHashProgress limit p = FileHashProgress
    { fhpBytesHashed    =         p
    , fhpBytesRemaining = limit - p }

readFile :: PS.MonadSafe m => FileInfo -> Producer FileChunk m ()
readFile info = PS.withFile (BC.unpack $ PF.filePath info) S.ReadMode PB.fromHandle

type FileHash = SHA256

hashFiles :: Monad m => Producer (FilePath, Producer FileChunk m ()) m () -> m FileHash
hashFiles fs = hashFileHashesP $ P.sequence <-< P.map hashFileChunksP <-< P.map snd <-< fs

hashFileChunksP :: Monad m => Producer FileChunk m () -> m FileHash
hashFileChunksP = F.purely P.fold SHA256.foldChunks

hashFileHashesP :: Monad m => Producer FileHash m () -> m FileHash
hashFileHashesP = F.purely P.fold SHA256.foldHashes

hashFilesSimple :: (PS.MonadSafe m, MonadIO m) => Pipe FileInfo FileHash m ()
hashFilesSimple = P.sequence <-< P.map (PS.runSafeT . hashFileChunksP . readFile)

hashFileTreeSimple :: FilePath -> IO FileHash
hashFileTreeSimple path = PS.runSafeT $ hashFileHashesP $
    hashFilesSimple <-< PF.descendantFiles PF.RootToLeaf path

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
    StreamStart f -> p { fthpFileCurrent = Just (PF.filePath f) }
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

type FileStreamEvent = StreamEvent FileInfo FileChunk

streamFileTree :: PS.MonadSafe m => FilePath -> Producer FileStreamEvent m ()
streamFileTree = PN.flatten . PN.nest (PF.descendantFiles PF.RootToLeaf) readFile

hashFileTree :: FilePath -> IO FileHash
hashFileTree = PS.runSafeT . hashFileStream . streamFileTree

hashFileTree'
    :: Monitor FileTreeHashProgress
    -> FilePath
    -> IO FileHash
hashFileTree' m f = PS.runSafeT $
    PP.withMonitor m initialFileTreeHashProgress (F.purely P.scan foldFileTreeHashProgress) $ \p ->
        hashFileStream (streamFileTree f >-> p)

hashFileStream :: Monad m => Producer FileStreamEvent m () -> m FileHash
hashFileStream = hashFileHashesP . PN.foldStreams SHA256.foldChunks

calculateDiskUsage'
    :: Monitor FileByteCount
    -> FilePath
    -> IO (Maybe FileByteCount)
calculateDiskUsage' m f = PS.runSafeT $
    PP.withMonitor m initialFileByteCount P.cat $ \p ->
        P.last $ PF.descendantFiles PF.RootToLeaf f
            >-> P.mapM (liftIO . getFileSize . PF.filePath)
            >-> F.purely P.scan foldFileByteCount
            >-> p

calculateDiskUsage
    :: FilePath
    -> IO FileByteCount
calculateDiskUsage f = PS.runSafeT $
    F.purely P.fold foldFileByteCount $
        PF.descendantFiles PF.RootToLeaf f
            >-> P.mapM (liftIO . getFileSize . PF.filePath)

countDescendantFiles
    :: FilePath
    -> IO Int
countDescendantFiles f = PS.runSafeT $
    P.length $
        PF.descendantFiles PF.RootToLeaf f

data FileByteCount = FileByteCount
    { fileCount :: {-# UNPACK #-} !FileCount
    , byteCount :: {-# UNPACK #-} !ByteCount } deriving Show

foldFileByteCount :: F.Fold ByteCount FileByteCount
foldFileByteCount = F.Fold
    updateFileByteCount
    initialFileByteCount
    id

updateFileByteCount :: FileByteCount -> ByteCount -> FileByteCount
updateFileByteCount p c = p
    { fileCount = fileCount p + 1
    , byteCount = byteCount p + c }
{-# INLINE updateFileByteCount #-}

initialFileByteCount :: FileByteCount
initialFileByteCount = FileByteCount
    { fileCount = 0
    , byteCount = 0 }

-- Experimental code below.

-- Event-based:
--
-- fileTree     :: FilePath -> Producer FilePathEvent       IO ()
-- hashFile     :: FilePath -> Producer HashFileEvent       IO FileHash
-- hashFileTree :: FilePath -> Producer HashFileTreeEvent   IO FileTreeHash
-- diskUsage    :: FilePath -> Producer FileByteCount       IO FileByteCount

-- Signal-based:

-- hashFileS     :: FilePath -> Producer HashFileProgress     IO FileHash
-- hashFileTreeS :: FilePath -> Producer HashFileTreeProgress IO FileTreeHash
-- diskUsageS    :: FilePath -> Producer FileByteCount        IO FileByteCount

-- Maybe the signal based thing can just be a context-dependent fold
--
-- class EventToSignal E S | E -> S where
--     foldEvents :: Fold E S
--
-- Then we have:
--
-- usage <- diskUsage path -> toSignal >-> samplePeriodically 1 >-> toConsole
-- hash <- hashFile path >-> toSignal >-> samplePeriodically 1 >-> toConsole
-- copyFile p q >-> samplePeriodically 1 >-> toConsole

-- Write a function:
--
-- samplePeriodically :: TimePeriod -> Producer Signal IO Result > Producer Signal IO Result
-- samplePeriodically :: TimePeriod -> Pipe Status Status IO Result
--
-- type EventSource e m r = Event e => Producer e m r
-- type SignalSource s m r = Signal s => Producer s m r
--
-- toSignal :: (Event e, Signal s) => Pipe e s m r

-- hashFile     :: FilePath -> FileHash
-- hashFileTree :: FilePath -> FileTreeHash
-- diskUsage    :: FilePath -> FileByteCount
--
-- maybe need some way to associate a timestamp with each moment
--
-- for each file in fileTree
--     yield $ FileOpen fileName
--     for each fileHashProgress in hashFile
--         yield $ FileRead size
--     yield $ FileClose fileName
--
-- perhaps the progress type could be combined with the result type?

data Terminated a = Value !a | End

instance Foldable Terminated where

    foldMap _ (End    ) = mempty
    foldMap f (Value x) = f x

    foldr _ y (End    ) =     y
    foldr f y (Value x) = f x y

    length (End    ) = 0
    length (Value _) = 1

    null (End    ) = True
    null (Value _) = False

terminated :: b -> (a -> b) -> Terminated a -> b
terminated e _ (End    ) = e
terminated _ f (Value v) = f v
{-# INLINABLE terminated #-}

signalLast :: Monad m => Producer a m r -> Producer (Terminated a) m s
signalLast p = (p >-> P.map Value) >> forever (yield End)

unsignalLast :: Monad m => Pipe (Terminated a) a m s
unsignalLast = P.concat
{-# INLINABLE unsignalLast #-}

foldReturnLast:: Monad m
    => (x -> a -> x) -> x -> (x -> b)
    -> Consumer (Terminated a) m b
foldReturnLast step begin done = loop begin where
    loop !x = await >>= terminated
        (pure $ done x)
        (loop . step x)
{-# INLINABLE foldReturnLast #-}

type HashFileProgressEvent = ByteCount
type HashFileProgress = ByteCount

openFile :: FilePath -> IOMode -> IO Handle
openFile = S.openFile . BC.unpack

hashFileX :: MonadSafe m => FilePath -> Producer HashFileProgressEvent m FileHash
hashFileX path = PS.bracket
    (liftIO $ openFile path S.ReadMode)
    (liftIO . S.hClose) $ \h ->
        signalLast (PB.fromHandle h)
        >-> P.tee (F.purely foldReturnLast SHA256.foldChunks)
        >-> unsignalLast >-> P.map (fromIntegral . B.length)

hashFileY :: MonadSafe m => FilePath -> Producer HashFileProgress m FileHash
hashFileY = (>-> P.scan (+) 0 id) . hashFileX

hashFileZ :: FilePath -> IO FileHash
hashFileZ = PS.runSafeT . drain . hashFileX

hashFileTreeX :: MonadSafe m => FilePath -> Producer HashFileTreeProgressEvent m FileHash
hashFileTreeX p =
    signalLast stream
        >-> P.tee (foldReturnLast step mempty id)
        >-> unsignalLast
    where
        step h = \case
            HashFileEnd g -> mappend h g
            _             -> h
        stream = for (PF.descendantFiles PF.RootToLeaf p) $ \i -> do
            let q = PF.filePath i
            yield $ HashFileStart q
            yield . HashFileEnd =<< hashFileX q >-> P.map HashFileChunk

hashFileTreeZ :: FilePath -> IO FileHash
hashFileTreeZ = PS.runSafeT . drain . hashFileTreeX

data HashFileTreeProgressEvent
    = HashFileStart !FilePath
    | HashFileChunk !ByteCount
    | HashFileEnd   !FileHash

data HashFileTreeProgress = HashFileTreeProgress
    { hftpFileCurrent :: Maybe FilePath
    , hftpFilesHashed :: !FileCount
    , hftpBytesHashed :: !ByteCount }

drain :: Monad m => Producer a m r -> m r
drain = runEffect . (>-> P.drain)

