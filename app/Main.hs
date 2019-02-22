module Main where

import Data.Text.IO as T
import DNest.Demo
import DNest.Walkable
import System.Random.MWC

main :: IO ()
main = withSystemRandom . asGenIO $ \rng -> do
    particle <- demoFromPrior rng
    T.putStrLn $ render particle

    particle' <- fst <$> perturb particle rng
    T.putStrLn $ render particle'

