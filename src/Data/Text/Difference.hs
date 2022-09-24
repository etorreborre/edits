-- | This module provides data types and functions to display
--   the difference between 2 strings according to the number of edit operations
--   necessary to go from one to the other
module Data.Text.Difference where

import Data.Text qualified as T
import Data.Text.Color
import Data.Text.EditOperation
import Data.Text.Shorten
import Data.Text.Token
import Protolude

-- | Separators are used to highlight a difference between 2 pieces of text
--   for example
data Separators = Separators
  { startSeparator :: Text,
    endSeparator :: Text
  }
  deriving (Eq, Show)

-- | Make parens separators
parensSeparators :: Separators
parensSeparators = makeCharSeparators '(' ')'

-- | Make brackets separators
bracketsSeparators :: Separators
bracketsSeparators = makeCharSeparators '[' ']'

-- | Make separators with simple Chars
makeCharSeparators :: Char -> Char -> Separators
makeCharSeparators c1 c2 = Separators (T.singleton c1) (T.singleton c2)

-- | Options to use for displaying differences
data DisplayOptions = DisplayOptions
  { _separators :: Separators,
    _shortenOptions :: ShortenOptions,
    _displayEditOperation :: EditOperation Char -> Text
  }

-- | Default display options
defaultDisplayOptions :: DisplayOptions
defaultDisplayOptions = DisplayOptions bracketsSeparators (ShortenOptions 20 "...") defaultDisplayEditOperations

-- | Display an edit operation by prepending a symbol showing which operation is used
defaultDisplayEditOperations :: EditOperation Char -> Text
defaultDisplayEditOperations (Insert c) = "+" <> T.singleton c
defaultDisplayEditOperations (Delete c) = "-" <> T.singleton c
defaultDisplayEditOperations (Substitute c1 c2) = "~" <> T.singleton c1 <> "/" <> T.singleton c2
defaultDisplayEditOperations (Keep c) = T.singleton c

-- | Display an edit operation using ascii colors: green = added, red = removed, blue = substituted
coloredDisplayEditOperation :: EditOperation Char -> Text
coloredDisplayEditOperation (Insert c) = colorAs Green (T.singleton c)
coloredDisplayEditOperation (Delete c) = colorAs Red (T.singleton c)
coloredDisplayEditOperation (Substitute c _) = colorAs Cyan (T.singleton c)
coloredDisplayEditOperation (Keep c) = T.singleton c

-- | Show the differences by enclosing them in separators
--   Additionally shorten the text outside the separators if it is too long
displayDiffs :: DisplayOptions -> [EditOperation Char] -> Text
displayDiffs (DisplayOptions (Separators start end) shortenOptions displayEditOperation) operations = do
  let (isDifferent, result) =
        foldl'
          ( \(different, res) operation ->
              --  different keeps track of the fact that we entered a section of the text having some differences
              --  this allows us to open a 'start' delimiter
              --  then when we go back to keeping the same character, we can close with an 'end' delimiter
              case operation of
                Insert {} -> (True, res <> [Delimiter start | not different] <> [Kept $ displayEditOperation operation])
                Delete {} -> (True, res <> [Delimiter start | not different] <> [Kept $ displayEditOperation operation])
                Substitute {} -> (True, res <> [Delimiter start | not different] <> [Kept $ displayEditOperation operation])
                Keep {} -> (False, res <> [Delimiter end | different] <> [Kept $ displayEditOperation operation])
          )
          (False, [])
          operations

  let fullResult = if isDifferent then result <> [Delimiter end] else result
  T.concat (showToken <$> shortenTokens shortenOptions (Delimiter start) (Delimiter end) fullResult)
