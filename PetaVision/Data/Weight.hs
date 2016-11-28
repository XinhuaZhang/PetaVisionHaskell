module PetaVision.Data.Weight where

import           Codec.Picture
import           Control.Monad   as M
import           Data.Array.Repa as R
import           Prelude         as P

-- the layout of the weigth array is nyp x nxp x nfp x numPatches
type PVPWeight = Array U DIM4 Double
type PVPWeightPatch = Array U DIM3 Double

-- normalize image pixel value to range of [0,upperBound]
normalizeWeightPatch
  :: Double -> PVPWeightPatch -> PVPWeightPatch
normalizeWeightPatch upperBound weightPatch =
  computeS $ R.map (\x -> (x - minV) / (maxV - minV) * upperBound) weightPatch
  where maxV = foldAllS max 0 weightPatch
        minV = foldAllS min 10000 weightPatch

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
                          (\i j ->
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
                          (\i j ->
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
       
plotWeightPatch :: FilePath -> PVPWeightPatch -> IO ()
plotWeightPatch filePath weightPatch =
  do let Z :. nyp' :. nxp' :. nfp' = extent weightPatch
         normalizedWeightPatch =
           normalizeWeightPatch (P.fromIntegral (maxBound :: Pixel8))
                                weightPatch
         w =
           case nfp' of
             1 ->
               ImageY8 $
               generateImage
                 (\i j ->
                    let v =
                          fromIntegral . round $
                          normalizedWeightPatch ! (Z :. j :. i :. 0)
                    in v)
                 nyp'
                 nxp'
             3 ->
               ImageRGB8 $
               generateImage
                 (\i j ->
                    let r =
                          fromIntegral . round $
                          normalizedWeightPatch ! (Z :. j :. i :. 0)
                        g =
                          fromIntegral . round $
                          normalizedWeightPatch ! (Z :. j :. i :. 1)
                        b =
                          fromIntegral . round $
                          normalizedWeightPatch ! (Z :. j :. i :. 2)
                    in PixelRGB8 r g b)
                 nyp'
                 nxp'
     savePngImage filePath w 


weightedSum
  :: PVPWeight -> Array U DIM1 Double -> Array U DIM3 Double
weightedSum weightPatches a = foldS (+) 0 arr
  where arr =
          traverse2 weightPatches
                    a
                    (\sh _ -> sh)
                    (\fwp fa sh@(Z :. _j :. _i :. _k :. n) ->
                       (fwp sh) * (fa (Z :. n)))
