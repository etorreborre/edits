module Test.Data.Text.EditsSpec where

import Data.Text.Edits
import Protolude
import Test.Tasty.Hedgehogx

test_show_distance = test "show the distance between different strings" $ do
  showDistance "kitte" "kittei" === ("kitte[]", "kitte[i]")
  showDistance "kitten" "kittein" === ("kitte[]n", "kitte[i]n")
  showDistance "kitten" "kit" === ("kit[ten]", "kit[]")
  showDistance "kit" "kitten" === ("kit[]", "kit[ten]")
  showDistance "kitten" "kitsin" === ("kit[te]n", "kit[si]n")
  showDistance "kitte" "kitte" === ("kitte", "kitte")
