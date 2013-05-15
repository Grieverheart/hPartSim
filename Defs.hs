module Defs
    (Particle(..)
    ,Configuration(..)
    ,Env(..)
    ,Variables(..)
    ,SimState(..)
    ) where

import Data.Vector            (Vector, toList)
import Vec3                   (Vec3)
import Box                    (Box)
import System.Random.Mersenne (MTGen)
import CellList               (CellList)

data Env a = Env {_nPart :: Int, _pressure :: a}

data Variables a = Variables
    {_dx   :: !a -- Maximum displacement
    ,_dv   :: !a -- Maximum volume change
    ,_nVol :: !Int -- Number of accepted Volume Moves
    ,_nMov :: !Int -- Number of accepted particle Moves
    }

data SimState a = SimState
    {_config   :: Configuration a
    ,_cellList :: CellList
    ,_vars     :: Variables a
    ,_rgen     :: MTGen
	}

data Particle a = Particle {_position :: Vec3 a} deriving(Show)

data Configuration a = Configuration{_box :: Box a, _particles :: Vector (Particle a)} -- A configuration is defined by the Box and the particles contained
instance (Show a) => Show (Configuration a) where
    show (Configuration box ps) = show box ++ "\n" ++ showParticles
      where showParticles = concatMap ((++"\n").show) (toList ps)