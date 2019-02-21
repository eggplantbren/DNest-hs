module Main where

import DNest.Demo
import System.Random.MWC

main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
    particle <- demoFromPrior rng
    print $ render particle

