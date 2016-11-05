module PetaVision.Data.Weight where

import           Codec.Picture
import           Control.Monad   as M
import           Data.Array.Repa as R
import           Prelude         as P

-- the layout of the weigth array is nyp x nxp x nfp x numPatches
type PVPWeight = Array U DIM4 Double

-- normalize image pixel value to range of [0,upperBound]
normalizeWeight
  :: Double -> PVPWeight -> PVPWeight
normalizeWeight upperBound weight =
  computeS $
  traverse3 weight
            minArr
            maxArr
            (\sh _ _ -> sh)
            (\fw fmin fmax sh@(Z :. j :. i :. k :. n) ->
               (fw sh - fmin (Z :. n)) / (fmax (Z :. n) - fmin (Z :. n)) *
               upperBound)
  where (Z :. nyp' :. nxp' :. nfp' :. numPatches') = extent weight
        permutedW =
          backpermute (Z :. numPatches' :. nyp' :. nxp' :. nfp')
                      (\(Z :. np :. j :. i :. k) -> Z :. j :. i :. k :. np)
                      weight
        bound = 1000000
        maxArr =
          foldS max (-bound) . foldS max (-bound) . foldS max (-bound) $
          permutedW
        minArr =
          foldS min bound . foldS min bound . foldS min bound $ permutedW

plotWeight :: FilePath -> PVPWeight -> IO ()
plotWeight filePath weight =
  do let Z :. nyp' :. nxp' :. nfp' :. numPatches' = extent weight
         normalizedW =
           normalizeWeight (P.fromIntegral (maxBound :: Pixel8))
                           weight
         weightPatches =
           case nfp' of
             3 ->
               P.map (\n ->
                        ImageRGB8 $
                        generateImage
                          (\j i ->
                             let r =
                                   fromIntegral . round $
                                   normalizedW ! (Z :. j :. i :. 0 :. n)
                                 g =
                                   fromIntegral . round $
                                   normalizedW ! (Z :. j :. i :. 1 :. n)
                                 b =
                                   fromIntegral . round $
                                   normalizedW ! (Z :. j :. i :. 2 :. n)
                             in PixelRGB8 r g b)
                          nyp'
                          nxp')
                     [0 .. (numPatches' - 1)]
             1 ->
               P.map (\n ->
                        ImageY8 $
                        generateImage
                          (\j i ->
                             let v =
                                   fromIntegral . round $
                                   normalizedW ! (Z :. j :. i :. 0 :. n)
                             in v)
                          nyp'
                          nxp')
                     [0 .. (numPatches' - 1)]
             _ ->
               error $ ("Image channel number is incorrect: " P.++ show nfp')
     M.zipWithM_
       (\n w ->
          savePngImage (filePath P.++ "/" P.++ show n P.++ ".png")
                       w)
       [0 .. (numPatches' - 1)]
       weightPatches


weightedSum :: PVPWeight -> Array U DIM1 Double -> Array U DIM3 Double
weightedSum weightPatches w 
  | 
  where Z :. nyp' :. nxp' :. nfp' :. numPatches' = extent weightPatches
        Z :. n' = extent w
