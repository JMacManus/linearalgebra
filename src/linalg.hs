module LinearAlgebra
( elem1
, elem2
, elem3
, gauss
, smith
) where

import Data.Matrix as M
import Data.Vector as V
import Prelude     as P

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

--[Smith normal form]-----------------------------------------------------------

-- TODO try analyse the complexity?

-- | returns the smith normal form of the matrix a, recursively.
--   Not massively tested but it works well from what I've seen.
smith :: Integral a => Matrix a -> Matrix a
smith a | isZero a                      = a
        | nrows a == 1 || ncols a == 1  = pivot a
        | otherwise = M.joinBlocks (tl,tr,bl,br') where
            (tl,tr,bl,br) = splitBlocks 1 1 $ pivot a
            br' = smith br

-- | This executes the first stage of snf.
--   a shouldn't be 0 at this point, and it's thankfully mathematically
--     impossible for that to happen halfway through the pivot.
--   Features a scrambling mechanism in case the first row and column
--     get annihilated before the divisiblity requirement is met. @Scramble a@
--     adds the first column to break the divisiblity rule to row 1.
pivot :: Integral a => Matrix a -> Matrix a
pivot = pivot' . moveSmallest where
    pivot' :: Integral a => Matrix a -> Matrix a
    pivot' a | a_11 < 0                      = pivot $ scaleRow (-1) 1 a
             | a_11 `dividesAll` a && zeros  = a
             | zeros                         = pivot $ scramble a
             | otherwise                     = a -: remBelow
                                                 -: M.transpose
                                                 -: remBelow
                                                 -: transpose
                                                 -: pivot
              where a_11 = a M.! (1,1)
                    zeros = isZero (V.tail (getCol 1 a)) &&
                            isZero (V.tail (getRow 1 a))
                    scramble a = combineRows 1 1 k a where
                        k = P.head $ P.filter (\j -> not
                                   $ a_11 `dividesAll` (getRow j a))
                                   [2..nrows a]

-- | checks whether a structure (a list, a matrix, a vector etc.) is all zeroes.
isZero :: (Foldable f, Num a, Eq a) => f a -> Bool
isZero = P.foldr (\a -> (&&) (a == 0)) True

-- | divides a b <=> a | b
divides :: Integral a => a -> a -> Bool
divides a b = b `rem` a == 0

-- | checks whether a divides every element in the foldable structure bs.
dividesAll :: (Foldable f, Integral a) => a -> f a -> Bool
dividesAll a bs = P.foldr (\b -> (&&) (a `divides` b)) True bs

-- | annihilates everything below (1,1), leaving only the remainder.
--   TODO read into applicative functors and maybe replace this weird foldr.
remBelow :: Integral a => Matrix a -> Matrix a
remBelow a = (P.foldr (.) id eros) a where
    eros = P.map (\(x,k) -> combineRows k (- x `div` a_11) 1 ) col'
    n        = nrows a
    a_11     = getElem 1 1 a
    colBelow = V.toList (V.drop 1 (getCol 1 a))
    col'     = P.zip colBelow [2 .. n]

-- | moves the smallest non-zero element to (1,1), assumes a is non-zero.
moveSmallest :: Integral a => Matrix a -> Matrix a
moveSmallest a = swap (1,1) (smallestIndex a) a

-- | note that switchCols' is being used here instead of the native due to
--   a bug in Data.Matrix.
swap :: (Int,Int) -> (Int,Int) -> Matrix a -> Matrix a
swap (i,j) (k,l) = (switchRows i k) . (switchCols' j l) where
    switchCols' j' l' = transpose . (switchRows j' l') . transpose

-- | returns the index of the entry with the smallest (non-zero) magnitude.
--   will error if passed the zero matrix.
smallestIndex :: Integral a => Matrix a -> (Int,Int)
smallestIndex a = snd $ P.minimum [(abs $ getElem i j a,(i,j))
                                   | i <- [1..m], j <- [1..n]
                                   , getElem i j a /= 0]
                                   where m = nrows a
                                         n = ncols a

--[Misc.]-----------------------------------------------------------------------

-- | for readability (and cos I don't understand monads)
(-:) :: a -> (a -> b) -> b
x -: f = f $ x
