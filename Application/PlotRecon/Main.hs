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

main = do
  reconFiles <- getArgs
  recons <- P.mapM (\reconFile -> runResourceT $ pvpFileSource reconFile $$ CL.head) reconFiles
  dir <- getCurrentDirectory
  createDirectoryIfMissing True (dir P.++ "/Recon")
  if P.any isNothing recons
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
               computeUnboxedS . L.foldl1' (+^) . L.map delay $
               arrs
         in do MP.sequence_ $
                 L.zipWith
                   (\fileName normalizedArr ->
                       plotWeightPatch
                         (dir P.++ "/Recon/" P.++ fileName P.++ ".png")
                         normalizedArr)
                   fileNames
                   normalizedArrs
               plotWeightPatch (dir P.++ "/Recon/recon.png") combinedArr
