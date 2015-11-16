{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pretty where

import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Data.Text as T

class Pretty x where pretty :: x -> Text

instance Pretty String where
    pretty = T.pack

prettyInteger :: (Num a, Ord a, Show a) => a -> Text
prettyInteger a =
    if a < 0
    then "-" <> prettyInteger (-a)
    else T.reverse
       $ T.intercalate ","
       $ T.chunksOf 3
       $ T.reverse
       $ T.pack
       $ show a

instance Pretty Integer where
    pretty = prettyInteger

