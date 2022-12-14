# `edits` [![Hackage](https://img.shields.io/hackage/v/edits.svg)](https://hackage.haskell.org/package/edits) [![Build Status](https://github.com/etorreborre/edits/workflows/ci/badge.svg)](https://github.com/etorreborre/edits/actions)


#### Presentation

This library provides a way to display the difference between 2 pieces of `Text` using the [Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance).

Here are a few examples:
```haskell
import Data.Text.Edits

-- -- "between the e and the n the letter i was added"
-- showDistance "kitten" "kittein" === "kitte[+i]n"
--
-- -- "at the end of the text 3 letters have been deleted"
-- showDistance "kitten" "kit" === "kit[-t-e-n]"
--
-- -- "between the t and the n 2 letters have been modified"
-- showDistance "kitten" "kitsin" === "kit[~t/s~e/i]"
```

Please have a look at the documentation of the [`Data.Text.Edits` module](https://github.com/etorreborre/edits/blob/main/src/Data/Text/Edits.hs) for the available configuration options.

One pre-configured function is showDistanceColored to use colors to show the edit operations
```haskell

import Data.Text.Edits

showDistanceColored "kitten" "kittein"
```
displays

<img src="doc/images/show-distance-colored.jpg" border="0"/>
