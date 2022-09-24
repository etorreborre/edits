{-# LANGUAGE RecordWildCards #-}

-- | This module supports a general definition of costs for performing edit operations
--   on 2 lists of elements
module Data.Text.Costs where

import Protolude

-- | Current operation in a cost matrix and current cost
data Cost
  = Insertion Int
  | Deletion Int
  | Substitution Int
  | NoAction Int
  deriving (Eq, Show)

-- | Return the cost of an operation
cost :: Cost -> Int
cost (Insertion c) = c
cost (Deletion c) = c
cost (Substitution c) = c
cost (NoAction c) = c

-- | Nicer display for a cost
showCost :: Cost -> Text
showCost (Insertion c) = "+ " <> show c
showCost (Deletion c) = "- " <> show c
showCost (Substitution c) = "~ " <> show c
showCost (NoAction c) = "o " <> show c

-- | This component contains functions to evaluate the cost of
--   substituting, inserting, deleting an element
data Costs a = Costs
  { substitutionCost :: a -> a -> Int,
    insertionCost :: a -> Int,
    deletionCost :: a -> Int,
    lowerCost :: a -> a -> Int -> Int -> Int -> Cost
  }

-- | Classic costs for the Levenshtein distance
--   applied to characters in a piece of Text
textLevenshteinCosts :: Costs Char
textLevenshteinCosts = levenshteinCosts @Char

-- | Classic costs for the Levenshtein distance
levenshteinCosts :: forall a. (Eq a) => Costs a
levenshteinCosts = Costs {..}
  where
    substitutionCost :: a -> a -> Int
    substitutionCost a1 a2 = if a1 == a2 then 0 else 1

    insertionCost :: a -> Int
    insertionCost = const 1

    deletionCost :: a -> Int
    deletionCost = const 1

    lowerCost :: a -> a -> Int -> Int -> Int -> Cost
    lowerCost a1 a2 del subst ins = do
      let (opDel, opSubst, opIns) = (Deletion del, Substitution subst, Insertion ins)
      if ins < del
        then (if (ins < subst) || (ins == subst && a1 == a2) then opIns else opSubst)
        else (if (del < subst) || (del == subst && a1 == a2) then opDel else opSubst)
