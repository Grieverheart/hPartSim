module Defs
    (Particle(..)
    ,Configuration(..)
    ) where

import Vec3 (Vec3)
import Box (Box)

data Particle a = Particle {position :: Vec3 a} deriving(Show)

data Configuration a = Configuration (Box a) [Particle a] -- A configuration is defined by the Box and the particles contained
instance (Show a) => Show (Configuration a) where
    show (Configuration box ps) = show box ++ "\n" ++ showParticles
      where showParticles = concatMap ((++"\n").show) ps
