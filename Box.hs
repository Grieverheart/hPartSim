module Box
    (Box
    ,scaleBox
    ,boxVolume
	,invertBox
    ) where

import Vec3           (Vec3)
import Data.Foldable  (product)
import Prelude hiding (product)

type Box a = Vec3 a

invertBox :: (Floating a) => Box a -> Box a
invertBox = fmap recip

boxVolume :: (Num a) => Box a -> a
boxVolume = product

scaleBox :: (Num a) => a -> Box a -> Box a
scaleBox ax = fmap (*ax)