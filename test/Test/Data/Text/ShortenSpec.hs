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
