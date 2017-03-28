module Main where

import           Codec.Picture
import           Control.Monad          as M
import           Control.Monad.Parallel as MP
import           Data.Array.Repa        as R
import           Data.Conduit
import           Data.Conduit.List      as CL
import           Data.List              as L
import           Data.Maybe             as Maybe
import           PetaVision.Data.Weight
import           PetaVision.PVPFile.IO
import           Prelude                as P
import           System.Directory
import           System.Environment
import           Control.Monad.Trans.Resource

main =
  do args <- getArgs
     let n = read . L.head $ args :: Int
         reconFiles = L.tail args
     recons <-
       P.mapM (\reconFile ->
                 runResourceT $ pvpFileSource reconFile $$ CL.take n)
              reconFiles
     dir <- getCurrentDirectory
     createDirectoryIfMissing True
                              (dir P.++ "/Recon")
     if L.any L.null recons
        then error "Read recon error"
        else let arrs =
                   L.map (\(PVP_OUTPUT_NONSPIKING_ACT (PVPDimension nx ny nf) y) ->
                            R.fromUnboxed (Z :. ny :. nx :. nf)
                                          y) .
                   L.map L.last $
                   recons
                 normalizedArrs =
                   L.map (normalizeWeightPatch (P.fromIntegral (maxBound :: Pixel8))) arrs
                 fileNames =
                   L.map (\reconFile ->
                            snd $
                            L.splitAt (L.last (L.findIndices (== '/') reconFile)) reconFile)
                         reconFiles
                 combinedArr =
                   normalizeWeightPatch (P.fromIntegral (maxBound :: Pixel8)) .
                   computeUnboxedS . L.foldl1' (+^) . L.map delay $
                   arrs
             in do MP.sequence_ $
                     L.zipWith (\fileName normalizedArr ->
                                  plotWeightPatch
                                    (dir P.++ "/Recon/" P.++ fileName P.++
                                     ".png")
                                    normalizedArr)
                               fileNames
                               normalizedArrs
                   plotWeightPatch (dir P.++ "/Recon/recon.png")
                                   combinedArr
