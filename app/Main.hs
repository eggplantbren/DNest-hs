module Main where

import DNest.Demo
import DNest.Sampler
import System.Random.MWC

main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do

    sampler <- initSampler 10 rng :: IO (Sampler DemoParticle)
    return ()

