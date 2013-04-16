import System.Environment
import System.Directory
import Control.Monad
import qualified Data.ByteString.Char8 as B
import ConfigParser
import Defs
import Vec3
import Box


applyBC :: (Fractional a, Ord a) => Vec3 a -> Vec3 a
applyBC vec = fmap applyBCsingle vec
    where applyBCsingle x
            | x > 1.0   = x - 1.0
            | x < 0.0   = x + 1.0
            | otherwise = x

checkCollision :: Box a -> (Particle a, Particle a) -> Bool
checkCollision = undefined

moveParticle :: (Fractional a) => Vec3 a -> Int -> Configuration a -> Configuration a
moveParticle dx nPart (Configuration box particles) = Configuration box (l ++ [particle'] ++ r)
  where (l, particle:r) = splitAt nPart particles
        particle'       = Particle $ position particle .+. dx

changeVolume :: (Fractional a) => a -> Configuration a -> Configuration a
changeVolume dv config@(Configuration box ps) =
    if isCollision
        then config
        else Configuration box' ps
  where v                   = boxVolume box
        box'                = scaleBox ((v+dv)/v) box
        isCollision         = and $ map (checkCollision box') (combinations ps)
        combinations []     = []
        combinations (x:xs) = [(x, y) | y <- xs] ++ combinations xs

main = do
    (filename:_) <- getArgs
    fileExists   <- doesFileExist filename
    when fileExists $ do
        content  <- B.readFile filename
        let configuration = loadConfig . B.lines $ content
        putStr $ show configuration