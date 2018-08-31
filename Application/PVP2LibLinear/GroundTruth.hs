module Application.PVP2LibLinear.GroundTruth where

import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           Data.Conduit                 as C
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Maybe
import           Data.Vector.Unboxed          as VU
import           PetaVision.PVPFile.IO
import           PetaVision.PVPFile.Types

groundTruthPVP2LabelTXT :: FilePath -> FilePath -> IO ()
groundTruthPVP2LabelTXT pvpFile txtFile = do
  xs <- runResourceT $ pvpFileSource pvpFile $$ CL.consume
  let ys =
        L.map
          (\(PVP_OUTPUT_NONSPIKING_ACT _ vec) ->
             show $ (fromJust $ VU.elemIndex 1 vec) + 1)
          xs
  writeFile txtFile . L.unlines $ ys
