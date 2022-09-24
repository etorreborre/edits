{-|
  This module can color some Text to be displayed in a terminal
-}
module Data.Text.Color where

import Protolude

-- | Available ASCII colors
data Color =
  Black | Red | Green | Yellow | Blue | Magenta | Cyan | White deriving (Eq, Show)

-- | Surround a piece of text with ASCII control characters to color it
colorAs :: Color -> Text -> Text
colorAs c t =  "\x1b[" <> code c <> "m" <> t <> "\x1b[0m"
  where
    code Black = "30"
    code Red = "31"
    code Green = "32"
    code Yellow = "33"
    code Blue = "34"
    code Magenta = "35"
    code Cyan = "36"
    code White = "37"
