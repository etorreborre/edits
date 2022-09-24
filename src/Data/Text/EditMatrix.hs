{-# LANGUAGE OverloadedLists #-}

-- | This module computes a matrix keeping track of the costs necessary
--   to edit one piece of text so that it is transformed into another one
--   From that matrix it is possible to extract the sequence of edit operations with the minimum cost
module Data.Text.EditMatrix where

import Data.Matrix hiding (matrix, (!), getElem, setElem)
import Data.Matrix qualified as M
import Data.Text.Costs
import Data.Text.EditOperation
import Data.Vector as V (Vector, cons, fromList, head, take, (!))
import Protolude

-- | Create a edit matrix where costs are computed using dynamic programming
createEditMatrix :: Costs a -> [a] -> [a] -> Matrix Cost
createEditMatrix costs as1 as2 = do
  let initialMatrix = M.matrix (length as1 + 1) (length as2 + 1) (const $ NoAction 0)
  let coordinates = cartesian (length as1) (length as2)
  foldl'
      ( \ma (i, j) ->
          let newCost
                | i == 0 = Insertion j -- no more letters for as1, we do j insertions
                | j == 0 = Deletion i -- no more letters for as2, we do i suppressions
                | otherwise = costOf i j ma -- otherwise we compute the cost and operation to go from as1[i] to as2[j]
           in setElement newCost i j ma
      )
      initialMatrix
      coordinates
  where
    (vs1, vs2) = (V.fromList as1, V.fromList as2)

    -- compute the cartesian product of the [0..i] x [0..j] lists
    cartesian :: Int -> Int -> [(Int, Int)]
    cartesian m n = [(i, j) | i <- [0 .. m], j <- [0 .. n]]

    -- compute the cost of going from as1[i] to as2[j], knowing the existing costs
    --  (i-1, j-1) (i-1, j)
    --  (i, j-1)   (i, j)
    --
    -- going from (i-1, j) to (i, j) means that we delete as1[i]
    -- going from (i-1, j-1) to (i, j) means that we substitute as1[i] with as2[j]
    -- going from (i, j-1) to (i, j) means that we insert as2[j]
    costOf :: Int -> Int -> Matrix Cost -> Cost
    costOf i j matrix = do
      let i1 = i - 1
      let j1 = j - 1
      let i1j =  getElement i1 j matrix
      let i1j1 = getElement i1 j1 matrix
      let ij1 =  getElement i j1 matrix
      let result =
            lowerCost
              costs
              (vs1 ! i1)
              (vs2 ! j1)
              (cost i1j + deletionCost costs (vs1 ! i1)) -- suppression
              (cost i1j1 + substitutionCost costs (vs1 ! i1) (vs2 ! j1)) -- substitution
              (cost ij1 + insertionCost costs (vs2 ! j1)) -- insertion
      -- in case of a substitution if the resulting cost of (i, j) is the same as (i-1, j-1)
      -- this means that we have substituted the same letter and it is the same as doing no action
      case result of
        Substitution {} | cost i1j1 == cost result -> NoAction (cost result)
        _ -> result

-- | From the original lists of characters, given the cost matrix
--   return a list of edit operations allowing to edit one text and eventually get the second one
makeEditOperations :: forall a. Vector a -> Vector a -> Matrix Cost -> Vector (EditOperation a)
makeEditOperations [] _ _ = []
makeEditOperations _ [] _ = []
makeEditOperations as1 as2 matrix =
  go (length as1) (length as2) []
  where
    go :: Int -> Int -> Vector (EditOperation a) -> Vector (EditOperation a)
    go 0 0 _ = []
    go i j ops = do
      let op = getElement i j matrix
      let dist = cost op
      if i == 1 && j == 1
        then
          if dist == 0
            then V.cons (Keep (V.head as1)) ops
            else V.cons (Substitute (V.head as1) (V.head as2)) ops
        else
          if j < 0
            then fmap Delete (V.take i as1) <> ops
            else
              if i < 0
                then fmap Insert (V.take j as2) <> ops
                else case op of
                  Insertion {} -> go i (j - 1) (V.cons (Insert (as2 ! (j - 1))) ops)
                  Deletion {} -> go (i - 1) j (V.cons (Delete (as1 ! (i - 1))) ops)
                  Substitution {} -> go (i - 1) (j - 1) (V.cons (Substitute (as1 ! (i - 1)) (as2 ! (j - 1))) ops)
                  _ -> go (i - 1) (j - 1) (V.cons (Keep (as1 ! (i - 1))) ops)

-- | Return the element at position i, j in the matrix
--   A matrix in the matrix package is 1-indexed but all the computations in this module are 0-indexed
--   so we need to shift the indices
getElement :: Int -> Int -> Matrix a -> a
getElement i j = M.getElem (i + 1) (j + 1)

-- | Set the element at position i, j in the matrix
--   A matrix in the matrix package is 1-indexed but all the computations in this module are 0-indexed
--   so we need to shift the indices
setElement :: a -> Int -> Int -> Matrix a -> Matrix a
setElement a i j = M.setElem a (i + 1, j + 1)
