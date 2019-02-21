{- Miscellaneous functions. -}

module DNest.Misc where

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

