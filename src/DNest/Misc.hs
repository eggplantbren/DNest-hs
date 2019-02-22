{- Miscellaneous functions. -}

module DNest.Misc where

-- Imports
import Control.Monad.Primitive
import System.Random.MWC
import System.Random.MWC.Distributions

-- Logsumexp
logSumExp :: Double -> Double -> Double
logSumExp a b = log (exp (a - xm) + exp (b - xm)) + xm where
  xm = max a b

-- Logdiffexp
logDiffExp :: Double -> Double -> Double
logDiffExp a b
    | b >= a = 0
    | otherwise = b + log (exp (a - b) - 1.0)

-- Mod
myMod :: Double -> Double -> Double
myMod y x = (y/x - ((fromIntegral :: Int -> Double) . floor) (y/x))*x

-- Wrap
wrap :: Double -> (Double, Double) -> Double
wrap x (a, b)
    | x < xMin || x > xMax = myMod (x - xMin) (xMax - xMin) + xMin
    | otherwise            = x
  where
    xMin = min a b
    xMax = max a b

-- Heavy tailed distribution
randh :: PrimMonad m => Gen (PrimState m) -> m Double
randh rng = do
    cauchy <- (\u -> tan (pi * (u - 0.5))) <$> uniform rng
    n <- standard rng
    return $ 10.0 ** (1.5 - abs cauchy) * n

