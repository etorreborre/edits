module Test.Data.Text.ShortenSpec where

import Data.Text qualified as T
import Data.Text.Shorten
import Data.Text.Token
import Protolude
import Test.Tasty.Hedgehogx

test_shorten = test "shorten difference" $ do
  shorten "abcd" === "abcd"
  shorten "abcdefghijkl[mn]opqr" === "...hijkl[mn]opqr"
  shorten "abcdefghijkl[mn]" === "...hijkl[mn]"
  shorten "[mn]abcdefghijkl" === "[mn]abcde..."
  shorten "abcdefghijkl[mn]opqrstuv" === "...hijkl[mn]opqrs..."
  shorten "hijkl[zz]abcdefghijklmno[xx]abcde" === "hijkl[zz]ab...no[xx]abcde"
  shorten "hijkl[]xxabcdefghijklmno[]xxabcde" === "hijkl[]xx...no[]xxabc..."
  shorten "abcdef[]ghijkl" === "...bcdef[]ghijk..."
  shorten "abcdefg[zz]abcdefghijklmno[xx]abcdefg" === "...cdefg[zz]ab...no[xx]abcde..."

test_split_on_delimiters = test "split on delimiters" $ do
  let start = Delimiter "("
  let end = Delimiter ")"
  -- ab[cd]ef[g]h
  let delimited = [Kept "a", Kept "b", start, Kept "c", Kept "d", end, Kept "e", Kept "f", start, Kept "g", end, Kept "h"]
  -- [[ab],[(cd)], [ef], [(g)], [h]]
  let expected =
        [ [Kept "a", Kept "b"],
          [start, Kept "c", Kept "d", end],
          [Kept "e", Kept "f"],
          [start, Kept "g", end],
          [Kept "h"]
        ]
  splitOnDelimiters start end delimited === expected

-- * Helpers

shorten :: Text -> Text
shorten t = do
  let tokens =
        toS t <&> \(c :: Char) ->
          if c == '['
            then Delimiter "["
            else
              if c == ']'
                then Delimiter "]"
                else Kept (T.singleton c)

  showTokens $ shortenTokens shortenOptions (Delimiter "[") (Delimiter "]") tokens
  where
    shortenOptions = ShortenOptions 5 "..."
