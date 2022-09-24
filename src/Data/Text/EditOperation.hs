-- | Data type describing the operation of editing some text
module Data.Text.EditOperation where

import Protolude

-- | Atomic operation required to edit a piece of text
--   at a given position in the EditMatrix
data EditOperation a
  = Insert a
  | Delete a
  | Substitute a a
  | Keep a
  deriving (Eq, Show)

-- | Inverse of an edit operation. It is used
--   to display not only how to go from text1 to text2 but also from text2 to text1
inverse :: EditOperation a -> EditOperation a
inverse (Insert a) = Delete a
inverse (Delete a) = Insert a
inverse (Substitute a1 a2) = Substitute a2 a1
inverse (Keep a) = Keep a
