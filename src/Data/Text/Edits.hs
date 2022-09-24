{-# LANGUAGE OverloadedLists #-}

-- |
--  This module provides a 'showDistance' function showing the differences between 2 pieces of text
--   using the Levenshtein distance. That distance is defined as the minimum number of edits: insertions, deletions, substitutions
--   to go from one text to another.
--
--   Several options are available to customize this processing:
--     - split size: texts are broken into new lines first. Then if the texts are too large there are split into smaller pieces
--       in order to compute their difference. This is done in order to reduce the size of the edit matrix which is used to compute all the edit costs
--       the default is 200
--
--     - separators: opening and closing pieces of text (brackets by default) used to highlight a difference
--
--     - shorten size: there is the possibly to display mostly the differences with a bit of context around if the input text is too large.
--       The text gets elided around separators if it gets greater than the shorten size (the default is 20)
--
--     - shorten text: the text to use when eliding characters in the original text (the default is "...")
--
--     - display edit operations: edit operations, insert/delete/substitute/keep can be annotated if necessary
--
-- Here are some examples:
--
-- @
-- import Data.Text.Edits
--
-- -- "between the e and the n the letter i was added"
-- showDistance "kitten" "kittein" === "kitte[+i]n"
--
-- -- "at the end of the text 3 letters have been deleted"
-- showDistance "kitten" "kit" === "kit[-t-e-n]"
--
-- -- "between the t and the n 2 letters have been modified"
-- showDistance "kitten" "kitsin" === "kit[~t/s~e/i]"
-- @
module Data.Text.Edits
  ( SplitSize (..),
    ShortenOptions (..),
    Separators (..),
    DisplayOptions (..),
    EditOperation (..),
    Color (..),
    colorAs,
    showDistance,
    showDistanceColored,
    showDistanceWith,
    levenshteinOperations,
    defaultDisplayOptions,
    defaultDisplayEditOperations,
    coloredDisplayEditOperation,
    defaultSplitSize,
    parensSeparators,
    bracketsSeparators,
    makeCharSeparators,
  )
where

import Data.Text qualified as T
import Data.Text.Color
import Data.Text.Costs
import Data.Text.Difference
import Data.Text.EditMatrix
import Data.Text.EditOperation
import Data.Text.Shorten
import Data.Vector qualified as V
import Protolude

-- | Size to use when splitting a large piece of text
newtype SplitSize = SplitSize Int deriving (Eq, Show)

-- | Default split size
defaultSplitSize :: SplitSize
defaultSplitSize = SplitSize 200

-- | Show the distance between 2 pieces of text
showDistance :: Text -> Text -> Text
showDistance = showDistanceWith defaultSplitSize defaultDisplayOptions

-- | Show the distance between 2 pieces of text with colors instead of symbols
showDistanceColored :: Text -> Text -> Text
showDistanceColored = showDistanceWith defaultSplitSize defaultDisplayOptions {_displayEditOperation = coloredDisplayEditOperation}

-- | Show the distance between 2 pieces of text and specify splitting / display options
showDistanceWith :: SplitSize -> DisplayOptions -> Text -> Text -> Text
showDistanceWith splitSize displayOptions ts1 ts2 =
  foldSplitTexts splitSize ts1 ts2 [] $ \ts (line1, line2) -> do
    let operations = levenshteinOperations (toS line1) (toS line2)
    ts <> displayDiffs displayOptions operations

-- | Return the list of operations necessary to go from one piece of text to another
--   using the Levenshtein distance
levenshteinOperations :: Text -> Text -> [EditOperation Char]
levenshteinOperations t1 t2 = do
  let matrix = createEditMatrix textLevenshteinCosts (toS t1) (toS t2)
  toList $ makeEditOperations (V.fromList . toS $ t1) (V.fromList . toS $ t2) matrix

-- | Split texts and apply the difference on each part
foldSplitTexts :: SplitSize -> Text -> Text -> a -> (a -> (Text, Text) -> a) -> a
foldSplitTexts splitSize t1 t2 initial f = do
  let (s1, s2) = (split splitSize t1, split splitSize t2)
  foldl' f initial (zip s1 s2)

-- | Split a text on newlines then split each line on a maximum split size
--   We then perform the edit distance algorithm on smaller sizes of text in order to control memory and CPU
split :: SplitSize -> Text -> [Text]
split splitSize = concatMap (splitToSize splitSize) . T.splitOn "\n"

-- | Split a text on a maximum split size
splitToSize :: SplitSize -> Text -> [Text]
splitToSize ss@(SplitSize n) t =
  if T.length t <= n
    then [t]
    else T.take n t : splitToSize ss (T.drop n t)
