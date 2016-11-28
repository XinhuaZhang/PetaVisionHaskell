module Main where

import           Codec.Picture
import           Data.Array.Repa        as R
import           Data.Conduit
import           Data.Conduit.List      as CL
import           PetaVision.Data.Weight
import           PetaVision.PVPFile.IO
import           Prelude                as P
import           System.Directory
import           System.Environment

main = do
  (reconFile:_) <- getArgs
  recon <- pvpFileSource reconFile $$ CL.head
  dir <- getCurrentDirectory
  removePathForcibly (dir P.++ "/Recon")
  createDirectoryIfMissing True (dir P.++ "/Recon/" P.++ folderName)
  case recon of
    Nothing -> error "Read recon error"
    Just (PVP_OUTPUT_NONSPIKING_ACT (PVPDimension nx ny nf) y) -> do
      let arr = R.fromUnboxed (Z :. ny :. nx :. nf) y
          normalizedWP =
            normalizeWeightPatch (P.fromIntegral (maxBound :: Pixel8)) arr
      plotWeightPatch (dir P.++ "/Recon/" P.++ reconFile P.++ ".png") normalizedWP
