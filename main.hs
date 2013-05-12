module Main (main) where

import System.Environment
import System.Directory
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Random
import Control.Monad.Reader
import Text.Printf
import Data.List (intercalate)
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as B (readFile, lines)
import ConfigParser
import Defs
import Vec3
import Box
import CellList


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
	}

generateCellList :: (RealFrac a) => Configuration a -> CellList
generateCellList (Configuration box particles) = V.foldr cllInsertAt' (CellList ncells (V.replicate (product ncells) [])) (V.indexed cell_indices)
  where ncells                   = map (truncate {--. (/ 0.5)--}) box
        cell_indices             = V.map (cellIndex ncells . _position) particles
        cllInsertAt' (i, ci) cll = cllInsertAt cll ci i

-- Change coords from fractional to normal
changeCoords :: (Floating a) => Box a -> Vec3 a -> Vec3 a
changeCoords box v = vec3fromList box .*. v

-- Calculate the distance between two particles given the periodic boundary conditions
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
    SimState config@(Configuration box particles) cll variables <- get
    let particle        = particles V.! nPart
        particle'       = Particle $ applyBC $ _position particle .+. dx
        cllIdx          = cellIndex (_nCells cll) (_position particle')
        isCollision     = any (checkCollision box) [(particle', particles V.! pId) | pId <- neighbours cll cllIdx, pId /= nPart]
    unless isCollision $ do --Accept move if there is no collision
        let cllIdxOld  = cellIndex (_nCells cll) (_position particle)
            cll'       = if cllIdx == cllIdxOld then cll else cllInsertAt (cllRemoveAt cll cllIdxOld nPart) cllIdx nPart
            particles' = modifyVectorElement nPart particle' particles
        put $! SimState config{_particles = particles'} cll' variables{_nMov = _nMov variables + 1}

changeVolume :: (Floating a, RealFrac a) => a -> Eval a ()
changeVolume dv = do
    SimState config@(Configuration box particles) cll variables <- get
    Env npart _                                                 <- ask
    let v                   = boxVolume box
        box'                = let scalef = ((v + dv) / v)**(1.0 / 3.0) in scaleBox scalef box
        isCollision         = any (checkCollision box') combinations
        {-- A list of all the particles pairs that need to be checked for collision.
        For each particle, the particles in the neighbouring cells are used. --}
        combinations        = do
            pId  <- [0..(npart - 1)]
            let particle = particles V.! pId
                cllIdx   = cellIndex (_nCells cll) (_position particle)
            pId' <- neighbours cll cllIdx
            guard (pId /= pId')
            return (particle, particles V.! pId')
    unless isCollision $ do --Accept move if there is no collision
        let new_cell_size = zipWith ((/) . fromIntegral) (_nCells cll) box'
            new_n         = map truncate box'
            config'       = config{_box = box'}
            recreate      = any (< 1.0) new_cell_size || any (> 2.0) new_cell_size || any (> 2) (zipWith ((abs .) . (-)) new_n (_nCells cll))
            cll'          = if recreate then generateCellList config' else cll
        put	$! SimState config' cll' variables{_nVol = _nVol variables + 1}

configToString :: (Floating a, Show a) => Configuration a -> String
configToString (Configuration b p) = boxToString b ++ particlesToString (V.toList p)
  where boxToString b             = intercalate "\t0.0\t0.0\t0.0\t" (map show b) ++ "\n"
        particlesToString         = concatMap (vec3ToString . changeCoords b . _position)
        vec3ToString (Vec3 x y z) = show x  ++ "\t" ++ show y ++ "\t" ++ show z ++ "\t0.0\t1.0\t0.0\t0.0\n"

type StateIO a = StateT (SimState a) IO
type Eval a = ReaderT (Env a) (StateIO a)
	
printConfig :: (Floating a, Show a) => FilePath -> Eval a ()
printConfig fp = do
    (SimState config _ _) <- get
    Env npart _           <- ask
    liftIO $ writeFile fp (show npart ++ "\n" ++ configToString config)

runSimulation :: Int -> Eval Double ()
runSimulation steps = do
    Env npart ps <- ask
    forM_ [1..steps] (\i -> do
        forM_ [1..npart+1] (\_ -> do
            SimState config _ variables <- get
            selection           <- liftIO $ getRandomR (0, npart)
            if selection < npart
                then do
                    rdx <- liftIO $ getRandomRs (0.0, _dx variables) >>= \r -> return $ take 3 r
                    rn  <- liftIO $ getRandomR (0, npart - 1)
                    lift $ moveParticle (vec3fromList rdx) rn
                else do
                    let dv = _dv variables
                    rdv   <- liftIO $ getRandomR (-dv, dv)
                    racc  <- liftIO $ getRandomR (0.0, 1.0)
                    let vol = boxVolume $ _box config
                        acc = exp $ (-ps) * rdv + fromIntegral npart * log ((vol + rdv) / vol)
                    when (racc < acc) $ changeVolume rdv
            )
        when (i `mod` 100 == 0) $ do
            state@(SimState _ _ variables) <- get
            let accVol = fromIntegral (_nVol variables) / 100.0
                accMov = fromIntegral (_nMov variables) / (fromIntegral npart * 100)
                olddx  = _dx variables
                olddv  = _dv variables
                newdx  = (*) olddx (if accMov > 0.3 && olddx < 0.45 then 1.04 else 0.94)
                newdv  = (*) olddv (if accVol > 0.15 then 1.04 else 0.94)
            liftIO $ print i
            liftIO $ printf "dx = %.6f, dv = %.6f, nMov = %d, nVol = %d\n" newdx newdv (_nMov variables) (_nVol variables)
            put $! state{_vars = variables{_dx = newdx, _dv = newdv, _nMov = 0, _nVol = 0}}
            printConfig $ "Data/test" ++ show i ++ ".dat"
        )
	

main :: IO ()
main = do
    (filename:_) <- getArgs
    fileExists   <- doesFileExist filename
    when fileExists $ do
        content  <- B.readFile filename
        let (n, configuration) = loadConfig . B.lines $ content
            cll                = generateCellList configuration
            env                = Env{_nPart = n, _pressure = 25.0}
            variables          = Variables{_dx = 0.1, _dv = 24.0, _nMov = 0, _nVol = 0}
        createDirectoryIfMissing False "Data"
        evalStateT (runReaderT (runSimulation 1000) env) (SimState configuration cll variables) 