module Test.Data.Text.EditsSpec where

import Data.Text.Edits
import Protolude
import Test.Tasty.Hedgehogx

test_show_distance = test "show the distance between different texts" $ do
  showDistance "kitte" "kittei" === "kitte[+i]"
  showDistance "kitten" "kittein" === "kitte[+i]n"
  showDistance "kitten" "kit" === "kit[-t-e-n]"
  showDistance "kit" "kitten" === "kit[+t+e+n]"
  showDistance "kitten" "kitsin" === "kit[~t/s~e/i]n"
  showDistance "kitte" "kitte" === "kitte"

test_edit_operations = test "list the edit operations as a difference between 2 texts" $ do
  levenshteinOperations "kitte" "kittei" === [Keep 'k', Keep 'i', Keep 't', Keep 't', Keep 'e', Insert 'i']
