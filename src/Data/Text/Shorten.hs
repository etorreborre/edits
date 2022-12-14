-- | This module provides functions to shorten a piece of text
--   where parts of the text are delimited to highlight the difference with another piece of text
--   Then only the parts outside the difference are being shortened
module Data.Text.Shorten where

import Data.Coerce
import Data.Text qualified as T
import Data.Text.Token
import Protolude

-- | Size used to decide if a piece of text needs to be shortened
data ShortenOptions = ShortenOptions
  { _shortenSize :: Int,
    _shortenText :: Text
  }
  deriving (Eq, Show)

-- | Cut the shorten size in 2
half :: ShortenOptions -> ShortenOptions
half (ShortenOptions ss t) = ShortenOptions (coerce ss `div` 2) t

-- | Shorten a piece of text that has already been tokenized
shortenTokens :: ShortenOptions -> Token -> Token -> [Token] -> [Token]
shortenTokens shortenOptions startDelimiter endDelimiter tokens =
  foldMap
    ( \ts ->
        case (head ts, lastMay ts) of
          -- <start>abcdefgh -> ...defgh
          (Just Start, _) -> shortenLeft shortenOptions ts
          -- abcdefgh<end> -> abcd...
          (_, Just End) -> shortenRight shortenOptions ts
          -- [abcdefgh] -> [abcdefgh]
          (Just s, Just e) | s == startDelimiter && e == endDelimiter -> ts
          -- abcdefgh -> abc...fgh
          _ -> shortenCenter shortenOptions ts
    )
    $ splitOnDelimiters startDelimiter endDelimiter (Start : (tokens <> [End]))

-- | Split a list of tokens into several lists when a delimiter is found
--   abcd[efgh]ijkl[mnop]qrst -> [abcd, [efgh], ijkl, [mnop], qrst]
splitOnDelimiters :: Token -> Token -> [Token] -> [[Token]]
splitOnDelimiters start end =
  foldl'
    ( \res cur ->
        if cur == start
          then res <> [[start]]
          else
            if cur == end
              then updateLast res (<> [end])
              else case lastMay res of
                Just ts ->
                  if lastMay ts == Just end
                    then res <> [[cur]]
                    else updateLast res (<> [cur])
                _ ->
                  [[cur]]
    )
    ([] :: [[Token]])

-- | Shorten some token on the left: ...tokens
shortenLeft :: ShortenOptions -> [Token] -> [Token]
shortenLeft so ts = whenTooLong so ts $ Kept (_shortenText so) : drop (length ts - _shortenSize so) ts

-- | Shorten some token on the right: tokens...
shortenRight :: ShortenOptions -> [Token] -> [Token]
shortenRight so ts = whenTooLong so ts $ take (_shortenSize so) ts <> [Kept $ _shortenText so]

-- | Shorten some token in the center: ...tokens...
shortenCenter :: ShortenOptions -> [Token] -> [Token]
shortenCenter so ts = whenTooLong so ts $ take (_shortenSize $ half so) ts <> [Kept $ _shortenText so] <> drop (length ts - _shortenSize so `div` 2) ts

-- | Depending on the shorten option and the original list of tokens used a shorter version
whenTooLong :: ShortenOptions -> [Token] -> [Token] -> [Token]
whenTooLong so original shortened =
  if tokenSize original > _shortenSize so then shortened else original
  where
    tokenSize :: [Token] -> Int
    tokenSize = sum . fmap (\case Kept value -> T.length value; _ -> 0)

-- * Helpers

-- | Update the last element of a list
updateLast :: [a] -> (a -> a) -> [a]
updateLast [] _ = []
updateLast [a] f = [f a]
updateLast (a : as) f = a : updateLast as f
