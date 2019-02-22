module Main where

-- Imports
import DNest.Demo
import DNest.Options
import DNest.Sampler
import System.Random.MWC

-- Main action
main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do

    -- Sampler for default example
    sampler <- initSampler defaultOptions rng :: IO (Sampler DemoParticle)
    return ()

