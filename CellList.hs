module CellList
    (CellList(..)
    ,cellIndex
	,neighbours
	,cllInsertAt
    ,cllRemoveAt
	,modifyVectorElement
    ) where

import qualified Data.Vector as V
import Data.Vector.Mutable (write)
import Vec3 (Vec3, vec3toList)

type Cell      = [Int] -- A cell contains a list of particle indices
type CellIndex = [Int] -- The index of a cell [i, j, k]

data CellList = CellList{_nCells :: [Int], _cells :: V.Vector Cell} deriving(Show)

-- Returns the cell index based on a position and the cell list shape (ncells)
cellIndex :: (RealFrac a) => [Int] -> Vec3 a -> CellIndex
cellIndex nCells !pos = zipWith ((truncate .) . (*)) posL (map fromIntegral nCells)
  where posL = vec3toList pos

  -- Get the Cell at the CellIndex
getCell :: CellList -> CellIndex -> Cell
getCell (CellList [ni, nj, nk] cll) [i, j, k] = cll V.! idx
  where !idx = nk * nj * i + nj * j + k

-- Returns the CellIndex's of all nearest neighbours at the CellIndex
cellNeighbours :: CellList -> CellIndex -> [CellIndex]
cellNeighbours (CellList !nCells _) cell = do
    i <- [-1..1]
    j <- [-1..1]
    k <- [-1..1]
    let temp = zipWith3 (((+).).(+)) nCells cell [i, j, k]
    return $! zipWith mod temp nCells

-- Returns a list of particle indices from the neighbouring cells
neighbours :: CellList -> CellIndex -> [Int]
neighbours cll !ci = concatMap (getCell cll) (cellNeighbours cll ci)

modifyVectorElement ::  Int -> a -> V.Vector a -> V.Vector a
modifyVectorElement !i !x !v = V.modify (\v' -> write v' i x) v

-- Insert element in the Cell of the given CellIndex
cllInsertAt :: CellList -> CellIndex -> Int -> CellList
cllInsertAt (CellList nCells@[ni, nj, nk] cll) [i, j, k] x = CellList nCells (modifyVectorElement idx (x : cell) cll)
  where !idx  = nk * nj * i + nj * j + k
        !cell = cll V.! idx

-- Remove element from the Cell of the given CellIndex
cllRemoveAt :: CellList -> CellIndex -> Int -> CellList
cllRemoveAt (CellList nCells@[ni, nj, nk] cll) [i, j, k] x = CellList nCells (modifyVectorElement idx (filter (/=x) cell) cll)
  where !idx  = nk * nj * i + nj * j + k
        !cell = cll V.! idx