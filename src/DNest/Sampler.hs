{- The sampler itself. -}

{-# LANGUAGE RecordWildCards #-}

module DNest.Sampler where

-- Imports
import Control.Monad.Primitive
import Control.Parallel (pseq)
import DNest.Options
import DNest.Walkable
import qualified Data.Vector as V
import System.Random.MWC

-- Sampler type
data Sampler a =
        Sampler
        {
            particles    :: !(V.Vector a),
            loglStore    :: ![Double],
            options      :: !Options
        }

-- Initialise a sampler
initSampler :: Walkable a
            => Options -> Gen RealWorld -> IO (Sampler a)
initSampler _options rng = do
    let options = _options
    putStr $ "Generating " ++ show (numParticles options)
                           ++ " particles from the prior..."
    particles <- V.replicateM (numParticles options) (fromPrior rng)
    let loglStore = V.toList $ V.map logLikelihood particles
    pseq loglStore $ putStrLn "done."
    return $ Sampler {..}


---- Explore for `threadSteps' number of MCMC steps
--explore :: (Walkable a, PrimMonad m)
--        => Sampler a -> Gen (PrimState m) -> m (Sampler a)
--explore Sampler {..} rng = loop (threadSteps options)
--    where
--        loop n
--            | n == 0 = return $! Sampler {..}
--            | otherwise = do
--                -- Choose a particle to move
--                k <- uniformR (0, numParticles options - 1) rng
--                (proposal, logH) <- perturb (particles V.! k) rng
--                let a = if logH > 0.0 then 1.0 else exp logH
--                u <- uniform rng
--                let updated = if u < a then proposal else (particles V.! k)
--                loop (n-1)

