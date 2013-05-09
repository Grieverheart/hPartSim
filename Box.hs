module Box
    (Box
    ,scaleBox
    ,boxVolume
	,invertBox
    ) where
	
type Box a = [a]

invertBox :: (Floating a) => Box a -> Box a
invertBox = map recip

boxVolume :: (Num a) => Box a -> a
boxVolume = product

scaleBox :: (Num a) => a -> Box a -> Box a
scaleBox ax = map (*ax)