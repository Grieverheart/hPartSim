{-# LANGUAGE ForeignFunctionInterface #-}
module ConfigParser (loadConfig) where

import Data.ByteString.Char8      (ByteString, words, useAsCString, readInt)
import Defs
import Box                        (Box, invertBox)
import Vec3                       (Vec3, vec3fromList, (.*.))
import Prelude hiding             (words)
import Foreign.C.Types            (CDouble(..))
import Foreign.C.String           (CString)
import System.IO.Unsafe           (unsafePerformIO)
import qualified Data.Vector as V (fromList)

foreign import ccall unsafe "stdlih atof" c_atof :: CString -> IO CDouble
bsToFloating :: (Floating a) => ByteString -> a
bsToFloating = realToFrac . unsafePerformIO . flip useAsCString c_atof

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where (first, rest) = splitAt n list

takeEvery :: Int -> [a] -> [a]
takeEvery _ [] = []
takeEvery n (x:xs) = case drop (n-1) xs of
    [] -> [x]
    ys ->  x : takeEvery n ys

readBox :: (Floating a) => ByteString -> Box a
readBox str = vec3fromList $ takeEvery 4 nums
  where nums = map bsToFloating (words str)

readParticle :: (Floating a) => Box a -> ByteString -> Particle a
readParticle inversebox str = Particle $ inversebox .*. pos
  where pos = vec3fromList . take 3 $ map bsToFloating (words str)
  
readNParticles :: ByteString -> Int
readNParticles str = case readInt str of
    Just (x, _) -> x
    _           -> error "Couldn't read file"

loadConfig :: (Floating a) => [ByteString] -> (Int, Configuration a)
loadConfig ls = (readNParticles nline, Configuration box (V.fromList $ map (readParticle $ invertBox box) body))
  where ([nline, bline], body) = splitAt 2 ls
        box = readBox bline