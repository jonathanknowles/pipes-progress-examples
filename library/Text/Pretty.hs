{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pretty where

import Data.Monoid ((<>))
import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Word as W

class Pretty x where pretty :: x -> Text

instance Pretty a => Pretty (Maybe a) where
    pretty (Nothing) = T.pack "<nothing>"
    pretty (Just x) = pretty x

instance Pretty W.Word64 where
    pretty = T.pack . show

instance Pretty String where
    pretty = T.pack

instance Pretty Integer where
    pretty = prettyInteger

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

