module Text.Pretty where

import Data.Text

class Pretty x where pretty :: x -> Text
