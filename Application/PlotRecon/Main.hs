module Main where

import           Codec.Picture
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

main = do
  reconFiles <- getArgs
  recons <- P.mapM (\reconFile -> pvpFileSource reconFile $$ CL.head) reconFiles
  dir <- getCurrentDirectory
  createDirectoryIfMissing True (dir P.++ "/Recon")
  if P.any isNothg recons
    then error "Read recon error"
    else let arrs =
               L.map
                 (\(PVP_OUTPUT_NONSPIKING_ACT (PVPDimension nx ny nf) y) ->
                     R.fromUnboxed (Z :. ny :. nx :. nf) y) .
               Maybe.catMaybes $
               recons
             normalizedArrs =
               L.map
                 (normalizeWeightPatch (P.fromIntegral (maxBound :: Pixel8)))
                 arrs
             fileNames =
               L.map
                 (\reconFile ->
                     snd $
                     L.splitAt (L.last (L.findIndices (== '/') reconFile)) reconFile)
                 reconFiles
             combinedArr =
               normalizeWeightPatch (P.fromIntegral (maxBound :: Pixel8)) .
               L.foldl1' (+^) $
               arrs
         in L.zipWithM_
              (\fileName normalizedArr ->
                  plotWeightPatch
                    (dir P.++ "/Recon/" P.++ fileName P.++ ".png")
                    normalizedArr)
              fileNames
              normalizedArrs
              plotWeightPatch
              (dir P.++ "/Recon/recon.png")
              combinedArr
