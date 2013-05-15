module Main (main) where

import           System.Environment         (getArgs)
import           System.Directory           (doesFileExist, createDirectoryIfMissing)
import           Control.Monad              (unless, when, guard, forM_, liftM)
import           Control.Monad.State.Strict (StateT, evalStateT, get, put)
import           System.Random.Mersenne     (random, randoms, MTGen, MTRandom, newMTGen)
import           Control.Monad.Reader       (ReaderT, runReaderT, ask)
import           Control.Monad.Trans
import           Control.Applicative        ((<$>))
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import qualified Data.Foldable as F         (product, any, foldr)
import           Text.Printf                (printf)
import           Data.List                  (intercalate)
import qualified Data.Vector as V           (foldr, replicate, indexed, (!), map, toList)
import qualified Data.ByteString.Char8 as B (readFile, lines)
import           ConfigParser
import           Defs
import           Vec3
import           Box
import           CellList

type StateIO a = StateT (SimState a) IO
type Eval a = ReaderT (Env a) (StateIO a)

getMTRandomR :: (Floating a, MTRandom a) => MTGen -> (a, a) -> IO a
getMTRandomR g (x, y) = ((+x) . (*(y - x))) <$> random g

getMTRandomRs :: (Floating a, MTRandom a) => MTGen -> (a, a) -> IO [a]
getMTRandomRs g !(x, y) = (((+x). (*(y - x))) <$>) <$> randoms g

getMTRandomRi :: (Integral a, MTRandom a) => MTGen -> (a, a) -> IO a
getMTRandomRi g (x, y) = ((+x) . (`mod` (y - x + 1))) <$> random g
	
generateCellList :: (RealFrac a) => Configuration a -> CellList
generateCellList (Configuration box particles) = V.foldr cllInsertAt' emptyCellList (V.indexed cell_indices)
  where ncells                   = fmap truncate box
        cell_indices             = V.map (cellIndex ncells . _position) particles
        cllInsertAt' (i, ci) cll = cllInsertAt cll ci i
        emptyCellList            = CellList ncells $ emptyCellVector $ F.product ncells

-- Change coords from fractional to normal
changeCoords :: (Floating a) => Box a -> Vec3 a -> Vec3 a
changeCoords box v = box .*. v

-- Calculate the distance between two particles given the periodic boundary conditions
distance :: (Ord a, Floating a) => Box a -> Vec3 a -> Vec3 a -> a
distance box x y = distVec `dot` distVec
  where distVec = changeCoords box $! fmap checkMin (x .-. y) 
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
moveParticle dx rId = do
    SimState config@(Configuration box particles) cll variables g <- get
    let particle        = particles V.! rId
        particle'       = Particle $ applyBC $ _position particle .+. dx
        cllIdx          = cellIndex (_nCells cll) (_position particle')
        isCollision     = any (checkCollision box) [(particle', particles V.! pId) | pId <- neighbours cll cllIdx, pId /= rId]
    unless isCollision $ do --Accept move if there is no collision
        let cllIdxOld  = cellIndex (_nCells cll) (_position particle)
            cll'       = if cllIdx == cllIdxOld then cll else cllInsertAt (cllRemoveAt cll cllIdxOld rId) cllIdx rId
            particles' = modifyVectorElement rId particle' particles
        put $! SimState config{_particles = particles'} cll' variables{_nMov = _nMov variables + 1} g

changeVolume :: (Floating a, RealFrac a) => a -> Eval a ()
changeVolume dv = do
    SimState config@(Configuration box particles) cll variables g <- get
    Env npart _                                                   <- ask
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
        let new_cell_size =  box' ./. fmap fromIntegral (_nCells cll)
            new_n         = fmap truncate box'
            config'       = config{_box = box'}
            recreate      = F.any (< 1.0) new_cell_size || F.any (> 2.0) new_cell_size || F.any ((> 2) . abs) (new_n .-. _nCells cll)
            cll'          = if recreate then generateCellList config' else cll
        put	$! SimState config' cll' variables{_nVol = _nVol variables + 1} g

configToString :: (Floating a, Show a) => Configuration a -> String
configToString (Configuration b p) = boxToString b ++ particlesToString (V.toList p)
  where boxToString               = F.foldr ((++) . (++ "\t0.0\t0.0\t0.0\t") . show) "\n"
        particlesToString         = concatMap (vec3ToString . changeCoords b . _position)
        vec3ToString (Vec3 x y z) = show x  ++ "\t" ++ show y ++ "\t" ++ show z ++ "\t0.0\t1.0\t0.0\t0.0\n"
	
printConfig :: (Floating a, Show a) => FilePath -> Eval a ()
printConfig fp = do
    (SimState config _ _ _) <- get
    Env npart _             <- ask
    liftIO $ writeFile fp (show npart ++ "\n" ++ configToString config)

runSimulation :: Int -> Eval Double ()
runSimulation steps = do
    Env npart ps <- ask
    forM_ [1..steps] (\i -> do
        forM_ [1..npart + 1] (\_ -> do
            SimState config _ variables g <- get
            selection                     <- liftIO $ getMTRandomRi g (0, npart)
            if selection < npart
                then do
                    rdx <- liftIO $ liftM (take 3) (getMTRandomRs g (0.0, _dx variables))
                    rn  <- liftIO $ getMTRandomRi g (0, npart - 1)
                    lift $ moveParticle (vec3fromList rdx) rn
                else do
                    let dv  = _dv variables
                    rdv  <- liftIO $ getMTRandomR g (-dv, dv)
                    racc <- liftIO $ random g
                    let vol = boxVolume $ _box config
                        acc = exp $ (-ps) * rdv + fromIntegral npart * log ((vol + rdv) / vol)
                    when (racc < acc) $ changeVolume rdv
            )
        when (i `mod` 100 == 0) $ do
            state@(SimState _ _ variables _) <- get
            let accVol = fromIntegral (_nVol variables) / 100.0
                accMov = fromIntegral (_nMov variables) / (fromIntegral npart * 100)
                olddx  = _dx variables
                olddv  = _dv variables
                newdx  = (*) olddx (if accMov > 0.3 && olddx < 0.45 then 1.04 else 0.94)
                newdv  = (*) olddv (if accVol > 0.15 then 1.04 else 0.94)
            put $! state{_vars = variables{_dx = newdx, _dv = newdv, _nMov = 0, _nVol = 0}}
            when (i `mod` 1000 == 0) $ do
                liftIO $ print i
                liftIO $ printf "dx = %.6f, dv = %.6f, nMov = %d, nVol = %d\n" newdx newdv (_nMov variables) (_nVol variables)
                printConfig $ "Data/test" ++ printf "%06d" i ++ ".dat"
        )
	

main :: IO ()
main = do
    (filename:_) <- getArgs
    fileExists   <- doesFileExist filename
    when fileExists $ do
        content  <- B.readFile filename
        seed     <- truncate <$> getPOSIXTime
        g        <- newMTGen $ Just seed
        let (n, configuration) = loadConfig . B.lines $ content
            cll                = generateCellList configuration
            env                = Env{_nPart = n, _pressure = 55.0}
            variables          = Variables{_dx = 0.1, _dv = 24.0, _nMov = 0, _nVol = 0}
        createDirectoryIfMissing False "Data"
        evalStateT (runReaderT (runSimulation 1000) env) (SimState configuration cll variables g)