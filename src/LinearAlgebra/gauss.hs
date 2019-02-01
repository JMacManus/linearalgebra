module LinearAlgebra.Gauss
( gauss
) where

import Data.Matrix as M
import Data.Vector as V
import Prelude     as P

--[Echelon form]----------------------------------------------------------------

-- | returns the index of the first non zero element in a vector
firstNonZero :: (Eq a, Num a) => Vector a -> Maybe Int
firstNonZero v = f (V.toList v) where
    f :: (Eq a, Num a) => [a] -> Maybe Int
    f [] = Nothing
    f (x:xs) | x == fromInteger 0  = f xs
             | otherwise           = Just (n - P.length xs)
    n = V.length v

-- | returns the echelon form of the input matrix
gauss :: (Eq a, Fractional a) => Matrix a -> Matrix a
gauss = g 1 1 where
    g :: (Eq a, Fractional a) => Int -> Int -> Matrix a -> Matrix a
    g i j m | outOfBounds       = m
            | pivot == Nothing  = g i (j+1) m
            | otherwise         = m -: switchRows i (pIndex+i-1)
                                    -: killBelow i j
                                    -: normalise i j
                                    -: g (i+1) (j+1)
            where pivot = firstNonZero (V.drop (i-1) (getCol j m))
                  pIndex = (\(Just k) -> k) pivot
                  outOfBounds = i > nrows m || j > ncols m

-- | killBelow takes a cell and an annihilates everything below
--    it using type 3 eros.
killBelow :: Fractional a => Int -> Int -> Matrix a -> Matrix a
killBelow i j m = (P.foldr (.) id eros) $ m where
    eros = P.map (\(x,k) -> combineRows k (- x / a_ij) i ) col'
    n        = nrows m
    a_ij     = getElem i j m
    colBelow = V.toList (V.drop i (getCol j m))
    col'     = P.zip colBelow [i+1 .. n]

-- | Takes in a cell and scales row in matrix to turn cell into 1.
normalise :: Fractional a => Int -> Int -> Matrix a -> Matrix a
normalise i j m = scaleRow (recip (getElem i j m)) i m

--[Misc.]-----------------------------------------------------------------------

-- | for readability (and cos I don't understand monads)
(-:) :: a -> (a -> b) -> b
x -: f = f $ x
