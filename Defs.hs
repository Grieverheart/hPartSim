module Defs
    (Particle(..)
    ,Configuration(..)
    ) where

import qualified Data.Vector as V (Vector, toList)
import Vec3 (Vec3)
import Box (Box)

data Particle a = Particle {_position :: Vec3 a} deriving(Show)

data Configuration a = Configuration{_box :: Box a, _particles :: V.Vector (Particle a)} -- A configuration is defined by the Box and the particles contained
instance (Show a) => Show (Configuration a) where
    show (Configuration box ps) = show box ++ "\n" ++ showParticles
      where showParticles = concatMap ((++"\n").show) (V.toList ps)