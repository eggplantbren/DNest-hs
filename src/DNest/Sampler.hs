{- The sampler itself. -}

{-# LANGUAGE ExistentialQuantification #-}

module DNest.Sampler where

-- Imports
import DNest.Walkable
import qualified Data.Vector as V

-- Sampler type
data Sampler a = Walkable a =>
        Sampler
        {
            numParticles :: !Int,
            particles    :: !(V.Vector a)
        }

