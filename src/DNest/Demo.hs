{- A trivial demo example. -}

{-# LANGUAGE RecordWildCards #-}

module DNest.Demo where

-- Imports
import Control.Monad.Primitive
import DNest.Misc
import DNest.Walkable
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import System.Random.MWC

-- Dimensionality
n :: Int
n = 10

-- Particle type
data DemoParticle =
        DemoParticle
        {
            xs :: !(U.Vector Double)
        } deriving (Eq, Read, Show)


-- Generate from prior
demoFromPrior :: PrimMonad m => Gen (PrimState m) -> m DemoParticle
demoFromPrior rng = do
    xs <- U.replicateM n (uniform rng)
    return $ DemoParticle {..}

-- Metropolis proposal
demoPerturb :: PrimMonad m
            => DemoParticle -> Gen (PrimState m) -> m (DemoParticle, Double)
demoPerturb DemoParticle {..} rng = do

    -- Choose coordinate to change
    k <- uniformR (0, U.length xs - 1) rng
    let x = xs U.! k
    x' <- (x + ) <$> randh rng
    let x'' = wrap x' (0.0, 1.0)

    xs' <- do
        mvec <- U.thaw xs
        UM.unsafeWrite mvec k x''
        U.unsafeFreeze mvec

    return $ (DemoParticle xs', 0.0)


-- Log likelihood
demoLogLikelihood :: DemoParticle -> Double
demoLogLikelihood DemoParticle {..} = U.foldl' (\acc x -> acc - 0.5*x*x) 0.0 xs

-- Render to text
demoRender :: DemoParticle -> T.Text
demoRender DemoParticle {..} =
    let
        strings = (\x -> show x ++ ",") <$> U.toList xs
        string  = mconcat strings
        string' = init string
    in
        T.pack string'

-- Typeclass instance    
instance Walkable DemoParticle where
    fromPrior = demoFromPrior
    logLikelihood = demoLogLikelihood
    perturb = demoPerturb
    render = demoRender

