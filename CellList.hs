module CellList
    (CellList(..)
    ,cellIndex
	,neighbours
	,cllInsertAt
    ,cllRemoveAt
	,modifyVectorElement
    ,emptyCellVector
    ) where

import Data.Vector         (Vector, (!), unsafeThaw, unsafeFreeze)
import Data.Vector.Mutable (write, replicate)
import Vec3                (Vec3(..), (.*.), (.+.), liftF2)
import Control.Monad.ST    (runST)
import Prelude hiding      (replicate)

type Cell      = [Int] -- A cell contains a list of particle indices
type CellIndex = Vec3 Int -- The index of a cell [i, j, k]

data CellList = CellList{_nCells :: Vec3 Int, _cells :: Vector Cell} deriving (Show)

emptyCellVector :: Int -> Vector Cell
emptyCellVector vsize = runST $ replicate vsize [] >>= unsafeFreeze

-- Returns the cell index based on a position and the cell list shape (ncells)
cellIndex :: (RealFrac a) => Vec3 Int -> Vec3 a -> CellIndex
{-# INLINE cellIndex #-}
cellIndex nCells !pos = fmap truncate $! pos .*. vNCells
  where vNCells = fmap fromIntegral nCells

  -- Get the Cell at the CellIndex
getCell :: CellList -> CellIndex -> Cell
getCell (CellList (Vec3 ni nj nk) cll) (Vec3 i j k) = cll ! idx
  where !idx = nk * nj * i + nj * j + k

-- Returns the CellIndex's of all nearest neighbours at the CellIndex
cellNeighbours :: CellList -> CellIndex -> [CellIndex]
cellNeighbours (CellList ncells _) ci = do
    ii <- neighbourOffsets
    return $! (ii .+. temp) `fmod` ncells
  where neighbourOffsets = [Vec3 i j k | i <- [-1..1], j <- [-1..1], k <- [-1..1]]
        fmod             = liftF2 mod
        temp             = ncells .+. ci

-- Returns a list of particle indices from the neighbouring cells
neighbours :: CellList -> CellIndex -> [Int]
neighbours cll !ci = concatMap (getCell cll) (cellNeighbours cll ci)

modifyVectorElement :: Int -> a -> Vector a -> Vector a
modifyVectorElement !i !x !v = runST $ unsafeThaw v >>= (\mv -> write mv i x >> unsafeFreeze mv)

-- Insert element in the Cell of the given CellIndex
cllInsertAt :: CellList -> CellIndex -> Int -> CellList
cllInsertAt cell_list@(CellList (Vec3 ni nj nk) cll) (Vec3 i j k) x = cell_list{_cells = modifyVectorElement idx (x : cell) cll}
  where !idx  = nk * nj * i + nj * j + k
        !cell = cll ! idx

-- Remove element from the Cell of the given CellIndex
cllRemoveAt :: CellList -> CellIndex -> Int -> CellList
cllRemoveAt cell_list@(CellList (Vec3 ni nj nk) cll) (Vec3 i j k) x = cell_list{_cells = modifyVectorElement idx (filter (/=x) cell) cll}
  where !idx  = nk * nj * i + nj * j + k
        !cell = cll ! idx