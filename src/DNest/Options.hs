{- A type that just contains options -}

module DNest.Options where

-- Options type
data Options =
        Options
        {
            numParticles :: !Int,
            threadSteps  :: !Int
        } deriving (Eq, Read, Show)


-- Default options to use generally
defaultOptions :: Options
defaultOptions = Options 10 100

