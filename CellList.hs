module CellList
    (CellList(..)
    ,cellIndex
	,neighbours
	,cllInsertAt
    ) where

import Vec3 (Vec3, vec3toList)

type Cell      = [Int] -- A cell contains a list of particle indices
type CellIndex = [Int] -- The index of a cell [i, j, k]

data CellList = CellList{_nCells :: [Int], _cells :: [Cell]} deriving(Show)

-- Returns the cell index based on a position and the cell list shape (ncells)
cellIndex :: (RealFrac a) => [Int] -> Vec3 a -> CellIndex
cellIndex nCells pos = zipWith ((truncate .) . (*)) posL (map fromIntegral nCells)
  where posL = vec3toList pos

getCell :: CellList -> CellIndex -> Cell
getCell (CellList [ni, nj, nk] cll) [i, j, k] = cll !! idx
  where idx = nk * nj * i + nj * j + k

cellNeighbours :: CellList -> CellIndex -> [CellIndex]
cellNeighbours (CellList nCells _) cell = tempL `seq` [zipWith (+) tempL [i, j, k] | i <- [-1..1], j <- [-1..1], k <- [-1..1]]
  where tempL = zipWith (+) nCells cell

-- Returns a list of particle indices from the neighbouring cells
neighbours :: CellList -> CellIndex -> [Int]
neighbours cll cp = concatMap (getCell cll) (cellNeighbours cll cp)

cllInsertAt :: CellList -> CellIndex -> Int -> CellList
cllInsertAt (CellList nCells@[ni, nj, nk] cll) [i, j, k] x = CellList nCells (l ++ (x : m) : r)
  where idx      = nk * nj * i + nj * j + k
        (l, m:r) = splitAt idx cll