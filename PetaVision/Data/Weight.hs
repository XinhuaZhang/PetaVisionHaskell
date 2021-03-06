{-# LANGUAGE BangPatterns #-}
module PetaVision.Data.Weight where

import           Codec.Picture
import           Control.Monad               as M
import qualified Data.Array                  as AU
import           Data.Array.Repa             as R
import           Data.List                   as L
import           Data.Vector.Unboxed         as VU
import           PetaVision.Data.Convolution
import           PetaVision.Utility.Parallel
import           PetaVision.Utility.Time
import           Prelude                     as P

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
          R.backpermute (Z :. numPatches' :. nyp' :. nxp' :. nfp')
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
                                   normalizedW R.! (Z :. j :. i :. 0 :. n)
                                 g =
                                   fromIntegral . round $
                                   normalizedW R.! (Z :. j :. i :. 1 :. n)
                                 b =
                                   fromIntegral . round $
                                   normalizedW R.! (Z :. j :. i :. 2 :. n)
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
                                   normalizedW R.! (Z :. j :. i :. 0 :. n)
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
                          normalizedWeightPatch R.! (Z :. j :. i :. 0)
                    in v)
                 nyp'
                 nxp'
             3 ->
               ImageRGB8 $
               generateImage
                 (\i j ->
                    let r =
                          fromIntegral . round $
                          normalizedWeightPatch R.! (Z :. j :. i :. 0)
                        g =
                          fromIntegral . round $
                          normalizedWeightPatch R.! (Z :. j :. i :. 1)
                        b =
                          fromIntegral . round $
                          normalizedWeightPatch R.! (Z :. j :. i :. 2)
                    in PixelRGB8 r g b)
                 nyp'
                 nxp'
             x -> error $ "plotWeightPatch: Image channel error: " L.++ show (extent weightPatch)
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


weightVisualization :: FilePath -> PVPWeight -> IO ()
weightVisualization filePath weight =
  do let Z :. nyp' :. nxp' :. nfp' :. numPatches' = extent weight
     M.sequence_
       [plotWeightPatch (filePath P.++ show np P.++ "_" P.++ show nf P.++ ".png") .
        computeS . extend (Z :. All :. All :. (1 :: Int)) . R.slice weight $
        (Z :. All :. All :. nf :. np)
       |np <- [0 .. numPatches' - 1]
       ,nf <- [0 .. nfp' - 1]]


weightListReconstruction :: ParallelParams -> [PVPWeight] -> [Int] -> PVPWeight
weightListReconstruction _ [] _ =
  error "weightListReconstruction: empty weight list."
weightListReconstruction _ ((!x):[]) _ =
  normalizeWeight (fromIntegral (maxBound :: Pixel8)) x
weightListReconstruction _ (x:y:_) [] =
  error "weightListReconstruction: empty stride list."
weightListReconstruction parallelParams (!x:(!y):xs) (s:ss) =
  weightListReconstruction parallelParams (newWeight : xs) ss
  where
    !newWeight = weightReconstruction parallelParams x y s

{-# INLINE weightReconstruction #-}

weightReconstruction
  :: ParallelParams -> PVPWeight -> PVPWeight -> Int -> PVPWeight
weightReconstruction parallelParams act weight stride =
  computeS .
  L.foldl1' R.append .
  L.map (R.extend (Z :. All :. All :. All :. (1 :: Int))) .
  parMap
    rseq
    (\z ->
       let arr = sumS .
                 L.foldl1' R.append .
                 L.map
                   (R.extend (Z :. All :. All :. All :. (1 :: Int)) . array2RepaArray3) .
                 L.zipWith
                   (\weight' y ->
                      let weightArr =
                            AU.listArray ((0, 0, 0), (wNy - 1, wNx - 1, wNf - 1)) .
                            R.toList $
                            weight' :: AU.Array (Int, Int, Int) Double
                          actArr = repaArray2Array2 y
                      in crossCorrelation25D stride actArr weightArr)
                   weightList $
                 z
       in deepSeqArray arr arr) $
  ys
  where
    (Z :. actNy :. actNx :. actNf :. actNumDict) = extent act
    (Z :. wNy :. wNx :. wNf :. wNumDict) = extent weight
    xs =
      L.map
        (\i -> R.slice act (Z :. All :. All :. All :. (i :: Int)))
        [0 .. actNumDict - 1]
    ys =
      L.map
        (\arr' ->
           L.map (\i -> R.slice arr' (Z :. All :. All :. i)) [0 .. actNf - 1])
        xs
    weightList =
      L.map
        (\i -> R.slice weight (Z :. All :. All :. All :. i))
        [0 .. wNumDict - 1]

{-# INLINE reorderWeight #-}

reorderWeight :: [Int] -> PVPWeight -> PVPWeight
reorderWeight idx w =
  computeS .
  R.backpermute
    (extent w)
    (\(Z :. ny :. nx :. nf :. np) -> (Z :. ny :. nx :. nf :. vec VU.! np)) $
  w
  where
    !vec = VU.fromList idx


{-# INLINE weightReconstructionAct #-}

weightReconstructionAct :: PVPWeightPatch -> PVPWeight -> Int -> PVPWeightPatch
weightReconstructionAct act weight stride =
  sumS .
  L.foldl1' R.append .
  L.map (R.extend (Z :. All :. All :. All :. (1 :: Int)) . array2RepaArray3) .
  parZipWith
    rdeepseq
    (\weight' y ->
       let weightArr =
             AU.listArray ((0, 0, 0), (wNy - 1, wNx - 1, wNf - 1)) . R.toList $
             weight' :: AU.Array (Int, Int, Int) Double
           actArr = repaArray2Array2 y
       in crossCorrelation25D stride actArr weightArr)
    weightList .
  L.map (\i -> R.slice act (Z :. All :. All :. i)) $
  [0 .. actNf - 1]
  where
    (Z :. actNy :. actNx :. actNf) = extent act
    (Z :. wNy :. wNx :. wNf :. wNumDict) = extent weight
    weightList =
      L.map
        (\i -> R.slice weight (Z :. All :. All :. All :. i))
        [0 .. wNumDict - 1]
