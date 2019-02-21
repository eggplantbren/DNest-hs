{- A trivial demo example. -}

{-# LANGUAGE RecordWildCards #-}

module DNest.Demo where

-- Imports
import Control.Monad.Primitive
import DNest.Walkable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
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


-- Log likelihood
demoLogLikelihood :: DemoParticle -> Double
demoLogLikelihood DemoParticle {..} = U.foldl' (\acc x -> acc - 0.5*x*x) 0.0 xs


-- Render to text
render :: DemoParticle -> T.Text
render DemoParticle {..} =
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

