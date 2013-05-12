module Main (main) where

import System.Environment
import System.Directory
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Random
import Control.Monad.Reader
import Data.List (intercalate)
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B
import ConfigParser
import Defs
import Vec3
import Box
import CellList


type Env = Int

data SimState a = SimState
    {_config   :: Configuration a
    ,_cellList :: CellList
	}

generateCellList :: (RealFrac a) => Configuration a -> CellList
generateCellList (Configuration box particles) = V.foldr cllInsertAt' (CellList ncells (V.replicate (product ncells) [])) (V.indexed cell_indices)
  where ncells                   = map (truncate {--. (/ 0.5)--}) box
        cell_indices             = V.map (cellIndex ncells . _position) particles
        cllInsertAt' (i, ci) cll = cllInsertAt cll ci i
	
changeCoords :: (Floating a) => Box a -> Vec3 a -> Vec3 a
changeCoords box v = vec3fromList box .*. v

distance :: (Ord a, Floating a) => Box a -> Vec3 a -> Vec3 a -> a
distance box x y = distVec `dot` distVec
  where distVec = changeCoords box $ fmap checkMin (x .-. y) 
        checkMin k
            | k >  0.5  = k - 1.0
            | k < -0.5  = k + 1.0
            | otherwise = k

applyBC :: (Floating a, Ord a) => Vec3 a -> Vec3 a
applyBC = fmap applyBCsingle
    where applyBCsingle x
            | x > 1.0   = applyBCsingle $ x - 1.0
            | x < 0.0   = applyBCsingle $ x + 1.0
            | otherwise = x

checkCollision :: (Floating a, Ord a) => Box a -> (Particle a, Particle a) -> Bool
checkCollision box (Particle x, Particle y) = distance box x y < 1.0

moveParticle :: (RealFrac a, Floating a) => Vec3 a -> Int -> StateIO a ()
moveParticle dx nPart = do
    SimState (Configuration box particles) cll <- get
    let particle        = particles V.! nPart
        particle'       = Particle $ applyBC $ _position particle .+. dx
        cllIdx          = cellIndex (_nCells cll) (_position particle')
        isCollision     = any (checkCollision box) [(particle', particles V.! pId) | pId <- neighbours cll cllIdx, pId /= nPart]
    unless isCollision $ do
        let cllIdxOld = cellIndex (_nCells cll) (_position particle)
            cll'      = if cllIdx == cllIdxOld then cll else cllInsertAt (cllRemoveAt cll cllIdxOld nPart) cllIdx nPart
        put $! SimState (Configuration box (modifyVectorElement nPart particle' particles)) cll'

changeVolume :: (Floating a, RealFrac a) => a -> Eval a ()
changeVolume dv = do
    SimState (Configuration box particles) cll <- get
    env                                        <- ask
    let v                   = boxVolume box
        box'                = let scalef = ((v + dv) / v)**(1.0 / 3.0) in scaleBox scalef box
        isCollision         = any (checkCollision box') combinations
        {-- A list of all the particles pairs that need to be checked for collision.
        For each particle, the particles in the neighbouring cells are used. --}
        combinations        = do
            pId  <- [0..(env - 1)]
            let particle = particles V.! pId
                cllIdx   = cellIndex (_nCells cll) (_position particle)
            pId' <- neighbours cll cllIdx
            guard (pId /= pId')
            return (particle, particles V.! pId')
    unless isCollision $ do
        let new_cell_size = zipWith ((/) . fromIntegral) (_nCells cll) box'
            new_n         = map truncate box'
            config'       = Configuration box' particles
            recreate      = any (< 1.0) new_cell_size || any (> 2.0) new_cell_size || any (> 2) (zipWith ((abs .) . (-)) new_n (_nCells cll))
            cll'          = if recreate then generateCellList config' else cll
        put	$! SimState config' cll'

configToString :: (Floating a, Show a) => Configuration a -> String
configToString (Configuration b p) = boxToString b ++ particlesToString (V.toList p)
  where boxToString b             = intercalate "\t0.0\t0.0\t0.0\t" (map show b) ++ "\n"
        particlesToString         = concatMap (vec3ToString . changeCoords b . _position)
        vec3ToString (Vec3 x y z) = show x  ++ "\t" ++ show y ++ "\t" ++ show z ++ "\t0.0\t1.0\t0.0\t0.0\n"

type StateIO a = StateT (SimState a) IO
type Eval a = ReaderT Env (StateIO a)
	
printConfig :: (Floating a, Show a) => FilePath -> Eval a ()
printConfig fp = do
    (SimState config _) <- get
    env                 <- ask
    liftIO $ writeFile fp (show env ++ "\n" ++ configToString config)

runSimulation :: Int -> Eval Double ()
runSimulation steps = do
    env <- ask
    forM_ [1..steps] (\i -> do
        forM_ [1..env+1] (\_ -> do
            SimState config _ <- get
            selection         <- liftIO $ getRandomR (0, env)
            if selection < env
                then do
                    dx <- liftIO $ getRandomRs (0.0, 0.01) >>= \r -> return $ take 3 r
                    n  <- liftIO $ getRandomR (0, env - 1)
                    lift $ moveParticle (vec3fromList dx) n
                else do
                    dv   <- liftIO $ getRandomR (-20.0, 20.0)
                    racc <- liftIO $ getRandomR (0.0, 1.0)
                    let vol = boxVolume $ _box config
                        acc = exp $ (-15.0 {-- should change to pressure --}) * dv + fromIntegral env * log ((vol + dv) / vol)
                    when (racc < acc) $ changeVolume dv
            )
        when (i `mod` 100 == 0) $ do
            liftIO $ print i
            printConfig $ "Data/test" ++ show i ++ ".dat"
        )
	

main :: IO ()
main = do
    (filename:_) <- getArgs
    fileExists   <- doesFileExist filename
    when fileExists $ do
        content  <- B.readFile filename
        let (n, configuration) = loadConfig . B.lines $ content
            cll = generateCellList configuration
        createDirectoryIfMissing False "Data"
        evalStateT (runReaderT (runSimulation 1000) n) (SimState configuration cll) 