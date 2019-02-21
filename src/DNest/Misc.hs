{- Miscellaneous functions. -}

module DNest.Misc where

-- Logsumexp
logsumexp :: Double -> Double -> Double
logsumexp a b = log (exp (a - xm) + exp (b - xm)) + xm where
  xm = max a b

-- Logdiffexp
logdiffexp :: Double -> Double -> Double
logdiffexp a b
    | b >= a = 0
    | otherwise = b + log (exp (a - b) - 1.0)

-- Mod
myMod :: Double -> Double -> Double
myMod y x = (y/x - (fromIntegral . floor) (y/x))*x

-- Wrap
wrap :: Double -> (Double, Double) -> Double
wrap x (a, b)
    | x < xmin || x > xmax = myMod (x - xmin) (xmax - xmin) + xmin
    | otherwise            = x
  where
    xmin = min a b
    xmax = max a b

