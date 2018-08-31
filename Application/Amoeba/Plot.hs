import           Control.Monad                as M
import           Control.Monad                as MP
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           PetaVision.Image.ImageIO
import           PetaVision.Utility.HDF5
import           System.Directory
import           System.Environment

{-# INLINE plotLabel #-}

plotLabel :: FilePath -> ImageRepa -> IO ()
plotLabel filePath =
  plotImageRepa filePath .
  normalizeImageRepa .
  fmap
    (computeS .
     R.map
       (\x ->
          if x == 255
            then 0
            else if x == 1
                    then 2
                    else 1))

{-# INLINE plotImage #-}

plotImage :: FilePath -> R.Array U DIM3 Double -> IO ()
plotImage filePath arr =
  let (Z :. nf :. rows :. cols) = extent arr
      maxIndexArr =
        fromListUnboxed (Z :. (1 :: Int) :. rows :. cols) -- .
        -- L.map
        --   (\k ->
        --      if k == 1
        --        then 1
        --        else 0) $
        [ fromIntegral . VU.maxIndex . toUnboxed . computeS . R.slice arr $
        (Z :. All :. i :. j)
        | i <- [0 .. rows - 1]
        , j <- [0 .. cols - 1]
        ]
      range =
        ( VU.minimum . toUnboxed $ maxIndexArr
        , VU.maximum . toUnboxed $ maxIndexArr)
  in do -- M.mapM_
        --   (\i ->
        --      plotImageRepa (filePath L.++ "_" L.++ show i L.++ ".png") .
        --      normalizeImageRepa .
        --      Image 8 .
        --      computeS . R.extend (Z :. (1 :: Int) :. All :. All) . R.slice arr $
        --      (Z :. i :. All :. All))
        --   [0 .. nf - 1]
        plotImageRepa (filePath L.++ ".png") . normalizeImageRepa . Image 8 $
          maxIndexArr
        print range

main = do
  (filePath:_) <- getArgs
  xs <-
    runResourceT . runConduit $
    hdf5Source1 filePath "label" "data" .| CL.consume
  removePathForcibly "Recon"
  createDirectory "Recon"
  MP.zipWithM_
     (\i (labelArr, imgArr) -> do
        plotLabel ("Recon/Label_" L.++ show i L.++ ".png") . Image 8 $ labelArr
        plotImage ("Recon/Recon_" L.++ show i) imgArr)
     [1 ..]
     xs
