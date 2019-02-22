{- The sampler itself. -}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

module DNest.Sampler where

-- Imports
import Control.Monad.Primitive
import Control.Parallel (pseq)
import DNest.Walkable
import qualified Data.Vector as V
import System.Random.MWC

-- Sampler type
data Sampler a = Walkable a =>
        Sampler
        {
            numParticles :: !Int,
            particles    :: !(V.Vector a),
            loglStore    :: ![Double]
        }

-- Initialise a sampler
initSampler :: Walkable a
            => Int -> Gen RealWorld -> IO (Sampler a)
initSampler numParticles rng = do
    putStr $ "Generating " ++ show numParticles
                           ++ " particles from the prior..."
    particles <- V.replicateM numParticles (fromPrior rng)
    let loglStore = V.toList $ V.map logLikelihood particles
    pseq loglStore $ putStrLn "done."
    return $ Sampler {..}

