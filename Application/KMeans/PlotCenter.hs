{-# LANGUAGE FlexibleContexts #-}
import           Application.KMeans.ArgsParser as Parser
import           Application.KMeans.Conduit
import           Application.PlotWeight.Grid
import           Codec.Picture
import           Control.Monad                 as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa               as R
import           Data.Array.Unboxed            as AU
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List             as CL
import           Data.List                     as L
import           Data.Vector                   as V
import           Data.Vector.Unboxed           as VU
import           PetaVision.Data.Convolution
import           PetaVision.Data.KMeans        as KMP
import           PetaVision.Data.Weight
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Array
import           PetaVision.Utility.Parallel
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


{-# INLINE repaArray2UArray3 #-}  

repaArray2UArray3
  :: (R.Source s Double)
  => R.Array s DIM3 Double -> UArray (Int, Int, Int) Double
repaArray2UArray3 arr =
  let (Z :. ny' :. nx' :. nf') = extent arr
  in listArray ((0, 0, 0), (ny' - 1, nx' - 1, nf'-1)) . R.toList $ arr

main =
  do (kmeansFile:weightFile:_) <- getArgs
     (KMeansModel (KMP.Shape actNy actNx actNf stride) cs kmeansModel) <-
       decodeFile kmeansFile
     weight <- runResourceT $ pvpFileSource weightFile $$ CL.head
     let sortedPairs =
           L.reverse . L.sortOn fst . V.toList $ V.zip cs kmeansModel
         xs =
           L.map (\x' ->
                    let arr' =
                          fromUnboxed (Z :. actNy :. actNx :. actNf)
                                      x'
                    in L.map (\i -> R.slice arr' (Z :. All :. All :. i))
                             [0 .. actNf - 1]) .
           L.map snd $ sortedPairs
         isConvolution =
           if actNy == 1
              then False
              else True
     case weight of
       Nothing -> error "Read weight error"
       Just (PVP_OUTPUT_KERNEL w) ->
         do let ys =
                  if isConvolution
                     then let (Z :. ny' :. nx' :. nf' :. numDict) = extent w
                              weightList =
                                L.map (\i ->
                                         R.slice w (Z :. All :. All :. All :. i))
                                      [0 .. numDict - 1]
                          in parMap rseq
                                    (sumS .
                                     L.foldl1' R.append .
                                     L.map (R.extend (Z :. All :. All :. All :.
                                                      (1 :: Int))) .
                                     L.zipWith (\weight' y ->
                                                  let weightArr =
                                                        repaArray2UArray3 weight'
                                                      actArr =
                                                        repaArray2UArray2 y
                                                  in uArray2RepaArray3 $
                                                     crossCorrelation25D stride actArr weightArr)
                                               weightList)
                                    xs
                     else parMap rseq
                                 (\zs ->
                                    let vec =
                                          fromUnboxed (Z :. (VU.length zs))
                                                      zs
                                    in sumS $
                                       traverse2 w
                                                 vec
                                                 const
                                                 (\fw fmu idx@(Z :. _j :. _i :. _k :. n) ->
                                                    fw idx * fmu (Z :. n))) .
                          L.map snd $ sortedPairs
            savePngImage
              "dictionary.png"
              (getGridImage $
               normalizeWeight (fromIntegral (maxBound :: Pixel8))
                               w)
            savePngImage
              "mean.png"
              (getGridImage .
               normalizeWeight (fromIntegral (maxBound :: Pixel8)) .
               computeS .
               L.foldl1' R.append .
               L.map (R.extend (Z :. All :. All :. All :. (1 :: Int))) $
               ys)
            M.mapM_ print . L.map fst $ sortedPairs
