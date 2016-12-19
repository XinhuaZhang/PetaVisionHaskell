module Main where

import           Application.FilterStats.FilterStats
import           Application.GMM.ConvertPVPGMMConduit
import           Control.Monad                        as M
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.Binary                  as CB
import           Data.Conduit.List                    as CL
import           Prelude                              as P
import           System.Environment

main = do
  (inputFile:_) <- getArgs
  xs <- runResourceT (sourceFile inputFile $$ featureConduit =$= CL.take 2)
  M.zipWithM_
    (\x i ->
        plotHist
          True
          x
          (valueRange x)
          1000
          (show i P.++ " " P.++ show (meanVar x))
          (show i P.++ ".png"))
    xs
    [1 ..]
