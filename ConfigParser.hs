{-# LANGUAGE ForeignFunctionInterface #-}
module ConfigParser (loadConfig) where

import qualified Data.ByteString.Char8 as B
import Defs
import Box (Box, invertBox)
import Vec3

import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe
import qualified Data.Vector as V (fromList)

changeCoords :: (Floating a) => Box a -> Vec3 a -> Vec3 a
changeCoords box v = vec3fromList box .*. v

foreign import ccall unsafe "stdlib.h atof" c_atof :: CString -> IO CDouble
bsToFloating :: (Floating a) => B.ByteString -> a
bsToFloating = realToFrac . unsafePerformIO . flip B.useAsCString c_atof

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where (first, rest) = splitAt n list

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n (x:xs) = case drop (n-1) xs of
    [] -> [x]
    ys ->  x : takeEvery n ys

readBox :: (Floating a) => B.ByteString -> Box a
readBox str = takeEvery 4 nums
  where nums = map bsToFloating (B.words str)

readParticle :: (Floating a) => Box a -> B.ByteString -> Particle a
readParticle inversebox str = Particle $ changeCoords inversebox pos
  where pos = vec3fromList . take 3 $ map bsToFloating (B.words str)
  
readNParticles :: B.ByteString -> Int
readNParticles str = case B.readInt str of
    Just (x, _) -> x
    _           -> error "Couldn't read file"

loadConfig :: (Floating a) => [B.ByteString] -> (Int, Configuration a)
loadConfig ls = (readNParticles nline, Configuration box (V.fromList $ map (readParticle $ invertBox box) body))
  where ([nline, bline], body) = splitAt 2 ls
        box = readBox bline