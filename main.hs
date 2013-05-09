module Main (main) where

import System.Environment
import System.Directory
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Random
import Control.Monad.Reader
import Data.List (intercalate)
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
generateCellList (Configuration box particles) = foldr cllInsertAt' (CellList ncells (replicate (product ncells) [])) (zip cell_indices [0..])
  where ncells                   = map (truncate . (/ 0.5)) box
        cell_indices             = map (cellIndex ncells . _position) particles
        cllInsertAt' (ci, i) cll = cllInsertAt cll ci i
	
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

moveParticle :: (Floating a, Ord a) => Vec3 a -> Int -> StateIO a ()
moveParticle dx nPart = do
    SimState (Configuration box particles) cll <- get
    let (l, particle:r) = splitAt nPart particles
        particle'       = Particle $ applyBC $ _position particle .+. dx
        isCollision     = any (checkCollision box) [(particle', p) | p <- l ++ r]
    unless isCollision $ do
        put $ SimState (Configuration box (l ++ particle':r)) cll

changeVolume :: (Floating a, Ord a) => a -> StateIO a ()
changeVolume dv = do
    SimState (Configuration box particles) cll <- get
    let v                   = boxVolume box
        box'                = let scalef = ((v + dv) / v)**(1.0 / 3.0) in scaleBox scalef box
        isCollision         = any (checkCollision box') (combinations particles)
        combinations []     = []
        combinations (x:xs) = [(x, y) | y <- xs] ++ combinations xs
    unless isCollision $ do
        put	$ SimState (Configuration box' particles) cll

configToString :: (Floating a, Show a) => Configuration a -> String
configToString (Configuration b p) = boxToString b ++ particlesToString p
  where boxToString b             = intercalate "\t0.0\t0.0\t0.0\t" (map show b) ++ "\n"
        particlesToString         = concatMap (vec3ToString . changeCoords b . _position)
        vec3ToString (Vec3 x y z) = show x  ++ "\t" ++ show y ++ "\t" ++ show z ++ "\t0.0\t1.0\t0.0\t0.0\n"

type StateIO a = StateT (SimState a) IO
type Eval = ReaderT Env (StateIO Double)
	
printConfig :: FilePath -> Eval ()
printConfig fp = do
    (SimState config _) <- get
    env                 <- ask
    liftIO $ writeFile fp (show env ++ "\n" ++ configToString config)

runSimulation :: Int -> Eval ()
runSimulation steps = do
    env <- ask
    forM_ [1..steps] (\i -> do
        liftIO $ print i
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
                    racc <- liftIO $ getRandomR (0.0, 1.0 :: Double)
                    let vol = boxVolume $ _box config
                        acc = exp $ (-15.0 {-- should change to pressure --}) * dv + fromIntegral env * log ((vol + dv) / vol)
                    when (racc < acc) $ lift $ changeVolume dv
            )
        when (i `mod` 100 == 0) $
            printConfig $ "Data/test" ++ show i ++ ".dat"
        )
	

main :: IO ()
main = do
    (filename:_) <- getArgs
    fileExists   <- doesFileExist filename
    when fileExists $ do
        content  <- B.readFile filename
        let (n, configuration) = loadConfig . B.lines $ content
        let cll = generateCellList configuration
        createDirectoryIfMissing False "Data"
        evalStateT (runReaderT (runSimulation 1000) n) (SimState configuration cll) 