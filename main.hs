module Main (main) where

import System.Environment
import System.Directory
import Control.Monad
import qualified Data.ByteString.Char8 as B
import ConfigParser
import Defs
import Vec3
import Box

toNormalCoords :: (Fractional a) => Vec3 a -> Box a -> Vec3 a
toNormalCoords v (Box box) = vec3fromList $ fmap (dot v . vec3fromList) box

distance :: (Ord a, Fractional a) => Box a -> Vec3 a -> Vec3 a -> a
distance box x y = distVec `dot` distVec
  where distVec = toNormalCoords (x .-. y) box 

applyBC :: (Fractional a, Ord a) => Vec3 a -> Vec3 a
applyBC = fmap applyBCsingle
    where applyBCsingle x
            | x > 1.0   = x - 1.0
            | x < 0.0   = x + 1.0
            | otherwise = x

checkCollision :: (Ord a, Fractional a) => Box a -> (Particle a, Particle a) -> Bool
checkCollision box (Particle x, Particle y) = distance box x y > 1.0

moveParticle :: (Fractional a, Ord a) => Vec3 a -> Int -> Configuration a -> Configuration a
moveParticle dx nPart config@(Configuration box particles) =
    if isCollision
	    then config
		else Configuration box (l ++ particle':r)
  where (l, particle:r) = splitAt nPart particles
        particle'       = Particle $ applyBC $ position particle .+. dx
        isCollision     = all (checkCollision box) [(particle', p) | p <- l ++ r]

changeVolume :: (Fractional a, Ord a) => a -> Configuration a -> Configuration a
changeVolume dv config@(Configuration box ps) =
    if isCollision
        then config
        else Configuration box' ps
  where v                   = boxVolume box
        box'                = scaleBox ((v+dv)/v) box
        isCollision         = all (checkCollision box') (combinations ps)
        combinations []     = []
        combinations (x:xs) = [(x, y) | y <- xs] ++ combinations xs

main = do
    (filename:_) <- getArgs
    fileExists   <- doesFileExist filename
    when fileExists $ do
        content  <- B.readFile filename
        let configuration = loadConfig . B.lines $ content
        putStr $ show configuration