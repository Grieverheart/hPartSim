module Box
    (Box(..)
    ,scaleBox
    ,boxVolume
    ) where
	
newtype Box a = Box [[a]] -- 3x3 Matrix
instance (Show a) => Show (Box a) where
    show (Box xs) = "Box\n" ++ concatMap ((++"\n").show) xs

boxVolume :: (Num a) => Box a -> a
boxVolume (Box box) = foldr (*) 1 [(box !! x) !! x | x <- [0..2]]

scaleBox :: (Num a) => a -> Box a -> Box a
scaleBox ax (Box box) = Box $ [l ++ ax * (head r) : tail r | x <- [0..2], let (l, r) = splitAt x (box !! x)]