-- | This module helps parsing some text with delimited differences
module Data.Text.Token where

import Protolude
import Data.Text qualified as T

-- | A Token is used to enclose a piece of text to compare and delimiters showing where the text is different from another piece of text
--   Start / End are markers for the beginning and end of that text
data Token
  = Kept Text
  | Delimiter Text
  | Start
  | End
  deriving (Eq, Show)

-- | Show a Token by skipping Start/End if present
showToken :: Token -> Text
showToken (Kept t) = t
showToken (Delimiter t) = t
showToken Start = ""
showToken End = ""

-- | Show a list of tokens. Start/End are skipped
showTokens :: [Token] -> Text
showTokens = T.concat . fmap showToken
