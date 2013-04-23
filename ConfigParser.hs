{-# LANGUAGE ForeignFunctionInterface #-}
module ConfigParser (loadConfig) where

import qualified Data.ByteString.Char8 as B
import Defs
import Box (Box, invertBox)
import Vec3

import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe

changeCoords :: (Floating a) => Box a -> Vec3 a -> Vec3 a
changeCoords box v = vec3fromList $ fmap (dot v . vec3fromList) box

foreign import ccall unsafe "stdlib.h atof" c_atof :: CString -> IO CDouble
bsToFloating :: (Floating a) => B.ByteString -> a
bsToFloating = realToFrac . unsafePerformIO . flip B.useAsCString c_atof

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where (first, rest) = splitAt n list

readBox :: (Floating a) => B.ByteString -> Box a
readBox str = splitEvery 3 nums
  where nums = map bsToFloating (B.words str)

readParticle :: (Floating a) => Box a -> B.ByteString -> Particle a
readParticle inversebox str = Particle $ changeCoords inversebox pos
  where pos = vec3fromList . take 3 $ map bsToFloating (B.words str)
  
readNParticles :: B.ByteString -> Int
readNParticles str = case B.readInt str of
    Just (x, _) -> x
    _           -> error "Couldn't read file"

loadConfig :: (Floating a) => [B.ByteString] -> (Int, Configuration a)
loadConfig ls = (readNParticles nline, Configuration box (map (readParticle $ invertBox box) body))
  where ([nline, bline], body) = splitAt 2 ls
        box = readBox bline