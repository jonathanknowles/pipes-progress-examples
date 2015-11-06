module Crypto.Hash.SHA256.Extra
    ( SHA256
    , foldChunks
    , foldHashes
    ) where

import Control.Foldl
    ( Fold (..) )
import Data.Bits
    ( xor )
import Data.ByteString
    ( ByteString )
import Text.Printf
    ( printf )

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString    as B
import qualified Data.Word          as W

data SHA256 = SHA256 !ByteString deriving Eq

instance Monoid SHA256 where
    mempty = SHA256 $ B.replicate 32 0
    mappend (SHA256 a) (SHA256 b) = SHA256 $ B.pack $ B.zipWith xor a b

instance Show SHA256 where
    show (SHA256 a) = concatMap showHex $ B.unpack a

showHex :: W.Word8 -> String
showHex = printf "%02x"

foldChunks :: Fold ByteString SHA256
foldChunks = Fold SHA256.update SHA256.init (SHA256 . SHA256.finalize)

foldHashes :: Fold SHA256 SHA256
foldHashes = Fold mappend mempty id

