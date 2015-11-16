module System.Posix.Files.ByteString.Extra where

import System.Posix.Types (FileOffset)
import System.Posix.ByteString (RawFilePath)

import qualified System.Posix.Files.ByteString as S

getFileSize :: Integral i => RawFilePath -> IO i
getFileSize = fmap (fromIntegral . S.fileSize) . S.getFileStatus

