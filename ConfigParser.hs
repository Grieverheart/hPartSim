{-# LANGUAGE ForeignFunctionInterface #-}
module ConfigParser (loadConfig) where

import qualified Data.ByteString.Char8 as B
import Defs
import Box (Box(..))
import Vec3 (vec3fromList)

import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe

foreign import ccall unsafe "stdlib.h atof" c_atof :: CString -> IO CDouble
bsToFractional :: (Fractional a) => B.ByteString -> a
bsToFractional = realToFrac . unsafePerformIO . flip B.useAsCString c_atof

splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where (first,rest) = splitAt n list

readBox :: (Fractional a) => B.ByteString -> Box a
readBox str = Box $ splitEvery 3 nums
  where nums = map bsToFractional (B.words str)

readParticle :: (Fractional a) => B.ByteString -> Particle a
readParticle str = Particle pos
  where pos = vec3fromList . take 3 $ map bsToFractional (B.words str)

loadConfig :: (Fractional a) => [B.ByteString] -> Configuration a
loadConfig ls = let (header, body) = splitAt 2 ls in
    Configuration (readBox $ header !! 1) (map readParticle body)