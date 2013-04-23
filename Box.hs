module Box
    (Box
    ,scaleBox
    ,boxVolume
	,invertBox
    ) where
	
type Box a = [[a]] -- 3x3 Matrix

mapBox :: (a -> a) -> Box a -> Box a
mapBox f = map (map f)

minorBox :: Int -> Int -> Box a -> Box a
minorBox i j = map (delm j) . delm i
  where delm k = (\(x, y) -> x ++ tail y) . splitAt k

cofactor :: (Floating a) => Int -> Int -> Box a -> a
cofactor i j = (*)((-1.0)**fromIntegral (i+j)) . determinant . minorBox i j

determinant :: (Floating a) => Box a -> a
determinant [[x]] = x
determinant box = sum $ zipWith (\a (x:_) -> a * x) [cofactor i 0 box | i <- [0..]] box

-- Just a matrix inverse
invertBox :: (Floating a) => Box a -> Box a
invertBox box = mapBox ((*) $ recip $ determinant box) [[cofactor i j box | i <- [0..n-1]] | j <- [0..n-1]]
  where n = length box

boxVolume :: (Num a) => Box a -> a
boxVolume box = product [(box !! x) !! x | x <- [0..2]]

scaleBox :: (Num a) => a -> Box a -> Box a
scaleBox ax box = [l ++ ax * head r : tail r | x <- [0..2], let (l, r) = splitAt x (box !! x)]