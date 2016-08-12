{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

module Pipes.Progress.Examples
    ( -- * Creating 'Monitor' instances
      terminalMonitor
      -- * Creating 'MonitorableEffect' instances
    , calculateDiskUsage
    , copyFile
    , copyTree
    , hashFile
    , hashTree
    ) where

import Control.Monad (forever)
import Control.Monad.Trans (MonadIO, liftIO)
import Crypto.Hash.SHA256.Extra (SHA256, foldChunks, foldHashes)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text)
import Pipes ((>->), (<-<), Consumer, Pipe, Producer, Proxy, await, for, runEffect, yield)
import Pipes.Core ((<\\), respond)
import Pipes.FileSystem (isFile, FileInfo, FileType (..))
import Pipes.Progress (Monitor (..), MonitorableEffect (..), TimePeriod (..), runMonitoredEffect)
import Pipes.Safe (MonadSafe, SafeT)
import Pipes.Termination (foldReturn, returnLastProduced, terminated)
import System.IO (IOMode, Handle)
import System.Posix.ByteString (RawFilePath)
import Text.Pretty
import Text.Printf (printf)

import Prelude hiding (FilePath)

import qualified Control.Foldl                       as F
import qualified Crypto.Hash.SHA256                  as SHA256
import qualified Crypto.Hash.SHA256.Extra            as SHA256
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Char8               as BC
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import qualified Data.Text.IO                        as T
import qualified Data.Word                           as W
import qualified Pipes                               as P
import qualified Pipes.ByteString                    as PB
import qualified Pipes.Extra                         as P
import qualified Pipes.FileSystem                    as PF
import qualified Pipes.Prelude                       as P
import qualified Pipes.Progress                      as PP
import qualified Pipes.Safe                          as PS
import qualified Pipes.Safe.Prelude                  as PS
import qualified System.IO                           as S
import qualified System.Posix.Directory.ByteString   as S
import qualified System.Posix.Files.ByteString       as S
import qualified System.Posix.Files.ByteString.Extra as S

----------------------------------------------
-- Supplementary types, classes and functions.
----------------------------------------------

-- Types and functions relating to processes, states, and state transitions.
----------------------------------------------------------------------------

{-| Represents a class of state transitions for a given process. Values of
    type 's' represent individual states, and values of type 'Transition s'
    represent transitions between pairs of consecutive states.
-}
class Update s where
    type Transition s
    update :: s -> Transition s -> s

-- Types and functions relating to general file and data processing.
--------------------------------------------------------------------

type FileChunk = ByteString
type FileHash  = SHA256
type FilePath  = RawFilePath

newtype ByteCount      = ByteCount      W.Word64 deriving (Enum, Eq, Integral, Num, Ord, Real, Show)
newtype DirectoryCount = DirectoryCount W.Word64 deriving (Enum, Eq, Integral, Num, Ord, Real, Show, Pretty)
newtype FileCount      = FileCount      W.Word64 deriving (Enum, Eq, Integral, Num, Ord, Real, Show, Pretty)

{-| Represents the current state of an operation that processes a single file. -}
data ProcessFileProgress = ProcessFileProgress
    { pfpBytesTarget    :: !(Maybe ByteCount)
    , pfpBytesProcessed :: !       ByteCount}

{-| Represents a change of state during an operation that processes a file. -}
data ProcessFileEvent
    = ProcessFileStart ByteCount FilePath
    | ProcessFileChunk ByteCount

instance Update ProcessFileProgress where
    type Transition ProcessFileProgress = ProcessFileEvent
    update p = \case
        ProcessFileStart size _ -> p { pfpBytesTarget = Just size }
        ProcessFileChunk size   -> p { pfpBytesProcessed = pfpBytesProcessed p + size }

instance Pretty FilePath where pretty = T.decodeUtf8

instance Pretty ByteCount where
    pretty (ByteCount a) =
        prettyInteger a <> " " <>
            if a == 1 || a == -1
                then "byte"
                else "bytes"

instance Pretty ProcessFileProgress where
    pretty ProcessFileProgress {..} = T.concat
        ["[", "processed: ", pretty pfpBytesProcessed, "]"
        , case pfpBytesTarget of
            Nothing -> ""
            Just bt -> T.concat
                [" ", "[", "remaining: ", pretty (bt - pfpBytesProcessed), "]" ] ]

openFile :: FilePath -> IOMode -> IO Handle
openFile = S.openFile . BC.unpack

----------------------------------------
-- Example implementations of 'Monitor'.
----------------------------------------

{-| A simple 'Monitor' implementation that echoes status updates to the
    the terminal at fixed intervals.

    The length of each interval is defined by the 'TimePeriod' argument.

    This implementation allows for early termination: if the underlying
    effect completes before the current interval is complete, then the
    monitor will echo the final status immediately without waiting.
-}
terminalMonitor :: (MonadSafe m, Pretty a) =>
    TimePeriod -> Monitor a m
terminalMonitor monitorStatusPeriod = Monitor {..} where
    monitorStatusUpdates = forever $ await >>= terminated
        (liftIO $ T.putStrLn "")
        (liftIO . T.putStr . (returnToStart <>) . pretty)

returnToStart :: Text
returnToStart = "\r\ESC[K"

--------------------------------------------------
-- Example implementations of 'MonitorableEffect'.
--------------------------------------------------

-- Example: Calculating disk usage.
-----------------------------------

{-| Calculate the disk space used by the file system object at the given
    path, including all files and directories that are descendants of the
    object.

    Returns 'mempty' if there is no file or directory at the given file path.
-}
calculateDiskUsage :: MonadSafe m
                   => FilePath -> MonitorableEffect DiskUsage m DiskUsage
calculateDiskUsage path = MonitorableEffect
    { effectStatusInitial = mempty
    , effectStatusUpdates = returnLastProduced mempty $
                            PF.descendants PF.RootToLeaf path
                            >-> P.mapM du >-> P.mscan }
    where du i = case PF.fileType i of
            PF.File ->   do s <- liftIO $ S.getFileSize $ PF.filePath i
                            pure $ mempty { duBytes       = s
                                          , duFiles       = 1 }
            PF.Directory -> pure $ mempty { duDirectories = 1 }
            _            -> pure   mempty

data DiskUsage = DiskUsage
    { duDirectories :: !DirectoryCount
    , duFiles       :: !FileCount
    , duBytes       :: !ByteCount }

instance Monoid DiskUsage where
    mempty = DiskUsage 0 0 0
    mappend (DiskUsage d1 f1 b1) (DiskUsage d2 f2 b2) =
        DiskUsage (d1 + d2) (f1 + f2) (b1 + b2)

instance Pretty DiskUsage where
    pretty DiskUsage {..} = T.concat
        [      "[","directories: ", pretty duDirectories, "]"
        , " ", "[",      "files: ", pretty duFiles      , "]"
        , " ", "[",      "bytes: ", pretty duBytes      , "]" ]

-- Example: Copying individual files.
-------------------------------------

{-| Copy a single file from the specified source path to the specified
    target path.
-}
copyFile :: MonadSafe m
    => FilePath -- ^ source path
    -> FilePath -- ^ target path
    -> MonitorableEffect ProcessFileProgress m ()
copyFile s t = MonitorableEffect
    { effectStatusUpdates = P.scan update start id <-< copyFileInner s t
    , effectStatusInitial = start }
    where start = ProcessFileProgress Nothing 0

copyFileInner :: MonadSafe m =>
    FilePath -> FilePath -> Producer ProcessFileEvent m ()
copyFileInner source target = PS.bracket
    (liftIO $ openFile source S.ReadMode)
    (liftIO . S.hClose) $ \i -> PS.bracket
        (liftIO $ openFile target S.WriteMode)
        (liftIO . S.hClose) $ \o -> do
            size <- liftIO $ S.getFileSize source
            yield $ ProcessFileStart (fromIntegral size) source
            PB.fromHandle i
                >-> P.tee (PB.toHandle o)
                >-> P.map (ProcessFileChunk . fromIntegral . B.length)

-- Example: Copying directory trees.
------------------------------------

{-| Copy a directory tree from the specified source path to the
    specified target path.
-}
copyTree :: MonadSafe m
    => FilePath -- ^ source path
    -> FilePath -- ^ target path
    -> MonitorableEffect CopyTreeProgress m ()
copyTree source target = MonitorableEffect
    { effectStatusUpdates = P.scan update start id <-< copyTreeInner source target
    , effectStatusInitial = start }
        where
    start = CopyTreeProgress Nothing 0 0 0

copyTreeInner :: MonadSafe m
    => FilePath -- ^ source path
    -> FilePath -- ^ target path
    -> Producer CopyTreeEvent m ()
copyTreeInner s t =
    for (PF.descendants PF.RootToLeaf s) $ \e -> do
        let i = PF.filePath e
            o = t </> relative s i
        case PF.fileType e of
            PF.Directory -> do
                liftIO $ S.getFileStatus i
                    >>= S.createDirectory o . S.fileMode
                yield $ CopyTreeDirectory i
            PF.File -> do
                yield $ CopyTreeFileStart i
                copyFileInner i o >-> P.map convert
                yield CopyTreeFileEnd
    where
        a </> b = a <> "/" <> b
        relative = B.drop . (+ 1) . B.length
        convert = \case
            ProcessFileStart s p -> CopyTreeFileStart p
            ProcessFileChunk s   -> CopyTreeFileChunk s

data CopyTreeEvent
    = CopyTreeDirectory !FilePath
    | CopyTreeFileStart !FilePath
    | CopyTreeFileChunk !ByteCount
    | CopyTreeFileEnd

instance Update CopyTreeProgress where
    type Transition CopyTreeProgress = CopyTreeEvent
    update a = \case
        CopyTreeDirectory p -> a { ctpDirectoriesCopied = ctpDirectoriesCopied a + 1 }
        CopyTreeFileStart p -> a { ctpFileCurrent       = Just p }
        CopyTreeFileChunk c -> a { ctpBytesCopied       = ctpBytesCopied a + c }
        CopyTreeFileEnd     -> a { ctpFilesCopied       = ctpFilesCopied a + 1
                                 , ctpFileCurrent       = Nothing }

data CopyTreeProgress = CopyTreeProgress
    { ctpFileCurrent       :: !(Maybe FilePath)
    , ctpFilesCopied       :: !FileCount
    , ctpBytesCopied       :: !ByteCount
    , ctpDirectoriesCopied :: !DirectoryCount }

instance Pretty CopyTreeProgress where
    pretty CopyTreeProgress {..} = T.concat
        [      "[", "directories copied: ", pretty ctpDirectoriesCopied, "]"
        ,      "[",       "files copied: ", pretty ctpFilesCopied      , "]"
        , " ", "[",       "bytes copied: ", pretty ctpBytesCopied      , "]"
        , case ctpFileCurrent of
            Nothing -> ""
            Just fc -> T.concat [" ", "[", "current: ", T.takeEnd 32 $ pretty fc, "]" ] ]

-- Example: Calculating hashes of individual files.
---------------------------------------------------

{-| Calculate the hash of the file at the specified path. -}
hashFile :: MonadSafe m => FilePath ->
    MonitorableEffect ProcessFileProgress m FileHash
hashFile path = MonitorableEffect
    { effectStatusUpdates = hashFileInner path >-> P.scan update start id
    , effectStatusInitial = start }
        where start = ProcessFileProgress Nothing 0

hashFileInner :: MonadSafe m => FilePath ->
    Producer ProcessFileEvent m FileHash
hashFileInner path = PS.bracket
    (liftIO $ openFile path S.ReadMode)
    (liftIO . S.hClose) $ \h -> do
        size <- liftIO $ S.getFileSize path
        yield $ ProcessFileStart size path
        F.purely foldReturn SHA256.foldChunks (PB.fromHandle h)
            >-> P.map (ProcessFileChunk . fromIntegral . B.length)

-- Example: Calculating hashes of directory trees.
--------------------------------------------------

{-| Calculate the hash of the file system object at the specified
    path by XOR-ing together the hashes of all the files that are
    descendants of the specified object. Directories are ignored. -}
hashTree :: MonadSafe m =>
    FilePath -> MonitorableEffect HashTreeProgress m FileHash
hashTree path = MonitorableEffect
    { effectStatusUpdates = hashTreeInner path >-> P.scan update start id
    , effectStatusInitial = start }
    where start = HashTreeProgress Nothing 0 0

hashTreeInner :: MonadSafe m =>
    FilePath -> Producer HashTreeEvent m FileHash
hashTreeInner p = foldReturn update mempty id stream where
    update h (HashTreeFileEnd g) = mappend h g
    update h (                _) = h
    stream = for (PF.descendantFiles PF.RootToLeaf p) $ \p -> do
        yield $ HashTreeFileStart p
        yield . HashTreeFileEnd =<<
            hashFileInner p >-> P.map convert
    convert = \case
        ProcessFileStart s p -> HashTreeFileStart p
        ProcessFileChunk s   -> HashTreeFileChunk s

data HashTreeProgress = HashTreeProgress
    { htpFileCurrent :: !(Maybe FilePath)
    , htpFilesHashed :: !FileCount
    , htpBytesHashed :: !ByteCount }

instance Pretty HashTreeProgress where
    pretty HashTreeProgress {..} = T.concat
        [      "[",       "files hashed: ", pretty htpFilesHashed      , "]"
        , " ", "[",       "bytes hashed: ", pretty htpBytesHashed      , "]"
        , case htpFileCurrent of
            Nothing -> ""
            Just fc -> T.concat [" ", "[", "current: ", T.takeEnd 32 $ pretty fc, "]" ] ]

data HashTreeEvent
    = HashTreeFileStart !FilePath
    | HashTreeFileChunk !ByteCount
    | HashTreeFileEnd   !FileHash

instance Update HashTreeProgress where
    type Transition HashTreeProgress = HashTreeEvent
    update a = \case
        HashTreeFileStart f -> a { htpFileCurrent = Just f }
        HashTreeFileChunk c -> a { htpBytesHashed = htpBytesHashed a + c }
        HashTreeFileEnd   h -> a { htpFileCurrent = Nothing
                                 , htpFilesHashed = htpFilesHashed a + 1 }

