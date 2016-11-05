{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}
module Application.GMM.Gaussian where

import           Application.GMM.Representation
import           Data.Binary
import           GHC.Generics

-- sigma is a diagonal matrix
data Gaussian =
  Gaussian {numDims :: Int
           ,mu      :: DataVec Double
           ,sigma   :: DataVec Double}
  deriving (Show,Generic)

instance Binary Gaussian where
  put (Gaussian numDims' mu' sigma') =
    do put numDims'
       put mu'
       put sigma'
  get =
    do numDims' <- get
       mu' <- get
       sigma' <- get
       return (Gaussian numDims' mu' sigma')

{-!x = (2 * pi) ** (0.5 * (fromIntegral numDims'))-}
gaussian
  :: Gaussian -> DataVec Double -> Double
gaussian (Gaussian numDims' mu' sigma') xs = result
  where !y = productVec sigma'
        !z = (-0.5) * (sumVec $ merge (/) (merge (-) xs mu') sigma')
        !result = (exp z) / y
