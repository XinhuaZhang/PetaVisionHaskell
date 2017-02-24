{-# LANGUAGE FlexibleContexts #-}
import           PetaVision.Utility.Array
import           Application.PlotWeight.Grid
import           Codec.Picture
import           Control.Monad                 as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa               as R
import           Data.Array.Unboxed            as AU
import           Data.Conduit
import           Data.Conduit.List             as CL
import           Data.List                     as L
import           Data.Vector.Unboxed           as VU
import           PetaVision.Data.Convolution
import           PetaVision.Data.Weight
import           PetaVision.PVPFile.IO
import           System.Environment

{-# INLINE uArray2RepaArray3 #-}

uArray2RepaArray3 :: UArray (Int, Int, Int) Double -> R.Array U DIM3 Double
uArray2RepaArray3 arr =
  let ((lb1, lb2, lb3), (ub1, ub2, ub3)) = bounds arr
  in fromListUnboxed
       (Z :. (ub1 - lb1 + 1) :. (ub2 - lb2 + 1) :. (ub3 - lb3 + 1)) .
     elems $
     arr

{-# INLINE repaArray2UArray2 #-}  

repaArray2UArray2
  :: (R.Source s Double)
  => R.Array s DIM2 Double -> UArray (Int, Int) Double
repaArray2UArray2 arr =
  let (Z :. ny' :. nx') = extent arr
  in listArray ((0, 0), (ny' - 1, nx' - 1)) . R.toList $ arr

main = do
  (s1WeightFile:s2WeightFile:_) <- getArgs
  s1Weight <- runResourceT $ pvpFileSource s1WeightFile $$ CL.head
  s2Weight <- runResourceT $ pvpFileSource s2WeightFile $$ CL.head
  let ys =
        case s2Weight of
          Nothing -> error "Read s2Weight error"
          Just (PVP_OUTPUT_KERNEL w) ->
            let (Z :. ny' :. nx' :. nf' :. numDict) = extent w
            in L.map
                 (\i -> R.slice w (Z :. All :. All :. All :. (i :: Int)))
                 [0 .. numDict - 1]
      xs =
        L.map
          (\arr' ->
              let (Z :. actNy :. actNx :. actNf) = extent arr'
              in L.map
                   (\i -> R.slice arr' (Z :. All :. All :. i))
                   [0 .. actNf - 1])
          ys
      stride = 1
  case s1Weight of
    Nothing -> error "Read weight error"
    Just (PVP_OUTPUT_KERNEL w) -> do
      let ys =
            let (Z :. ny' :. nx' :. nf' :. numDict) = extent w
                weightList =
                  L.map
                    (\i -> R.slice w (Z :. All :. All :. All :. i))
                    [0 .. numDict - 1]
            in L.zipWith
                 (\x weight' ->
                     sumS .
                     L.foldl1' R.append .
                     L.map (R.extend (Z :. All :. All :. All :. (1 :: Int))) .
                     L.map
                       (\y ->
                           let weightArr =
                                 listArray
                                   ((0, 0, 0), (ny' - 1, nx' - 1, nf' - 1)) .
                                 R.toList $
                                 weight' :: UArray (Int, Int, Int) Double
                               actArr = repaArray2UArray2 y
                           in uArray2RepaArray3 $
                              crossCorrelation25D stride actArr weightArr) $
                     x)
                 xs
                 weightList
      savePngImage
        "dictionary.png"
        (getGridImage $ normalizeWeight (fromIntegral (maxBound :: Pixel8)) w)
      savePngImage
        "mean.png"
        (getGridImage .
         normalizeWeight (fromIntegral (maxBound :: Pixel8)) .
         computeS .
         L.foldl1' R.append .
         L.map (R.extend (Z :. All :. All :. All :. (1 :: Int))) $
         ys)
      plotWeightPatch
        "sum.png"
        (sumS .
         L.foldl1' R.append .
         L.map (R.extend (Z :. All :. All :. All :. (1 :: Int))) $
         ys)
