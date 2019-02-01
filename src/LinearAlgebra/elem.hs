module LinearAlgebra.Elem
( elem1
, elem2
, elem3
) where

import Data.Matrix as M

--[Elementary Matrices]---------------------------------------------------------

-- | elem1 corresponds to swapping rows i and j:
elem1 :: Num a => Int -> Int -> Int -> Matrix a
elem1 n i j = switchRows i j (identity n)

-- | elem2 corresponds to scaling row i by c
elem2 :: Num a => Int -> a -> Int -> Matrix a
elem2 n c i = scaleRow c i (identity n)

-- | elem3 corresponds to adding c times row j to row i:
elem3 :: Num a => Int -> Int -> a -> Int -> Matrix a
elem3 n i c j = combineRows i c j (identity n)
