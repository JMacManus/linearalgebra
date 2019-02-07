{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.List
import qualified Data.Matrix as M (prettyMatrix, Matrix, fromLists)

-- | type synomyms
type Matrix a = [[a]]
type Vector a =  [a]

-- | doesn't seem to want to display stuff properly
instance {-# OVERLAPPING #-} Show a => Show (Matrix a) where
    show = M.prettyMatrix . M.fromLists

-- e.g. [[1,2,3],[4,5,6],[7,8,9]]
--     | 1 2 3 |
--   = | 4 5 6 |
--     | 7 8 9 |

--[arbitrary matrices]------------------------------------------------------------

ass :: Matrix Int
ass = [[1,2,3],[4,5,6],[7,8,9]]

--[Vector operations]-----------------------------------------------------------

-- | vector addition, component-wise
vectAdd   :: Num a => Vector a -> Vector a -> Vector a
vectAdd = zipWith (+)

-- | scalar multiplication, component-wise.
vectScale :: Num a => a -> Vector a -> Vector a
vectScale c = map (* c)

--[Matrix properties]-----------------------------------------------------------

rows :: Matrix a -> Int
rows = length

cols :: Matrix a -> Int
cols mss = length (mss !! 0)

row :: Matrix a -> Int -> Vector a
row mss i = mss !! (i - 1)

--[EROs and ECOs]---------------------------------------------------------------

-- | type 1 ERO, swaps row i with row j in matrix mss.
swapRows    :: Int -> Int -> Matrix a -> Matrix a
swapRows i j mss = [get k | k <- [1..rows mss]] where
    get k | k == i     = mss !! (j - 1)
          | k == j     = mss !! (i - 1)
          | otherwise  = mss !! (k - 1)

-- | type 2 ERO, scales row i by scalar c in matrix mss
scaleRow    :: Num a => a -> Int -> Matrix a -> Matrix a
scaleRow c i mss = [get k | k <- [1..rows mss]] where
    get k | k == i     = vectScale c (mss !! (i - 1))
          | otherwise  = (mss !! (k - 1))

-- | type 3 ERO, adds c times row j to row i in matrix mss
combineRows :: Num a => Int -> a -> Int -> Matrix a -> Matrix a
combineRows i c j mss = [get k | k <- [1..rows mss]] where
    get k | k == i     = vectAdd (mss !! (i - 1)) (vectScale c (mss !! (j - 1)))
          | otherwise  = (mss !! (k - 1))

-- | type 1 ECO
swapCols    :: Int -> Int -> Matrix a -> Matrix a
swapCols i j = transpose . (swapCols i j) . transpose

-- | type 2 ECO
scaleCol    :: Num a => a -> Int -> Matrix a -> Matrix a
scaleCol c i = transpose . (scaleCol c i) . transpose

-- | type 3 ECO
combineCols :: Num a => Int -> a -> Int -> Matrix a -> Matrix a
combineCols i c j = transpose . (combineRows i c j) . transpose
