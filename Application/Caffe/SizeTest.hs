import           Application.Caffe.LMDB
import           Control.Arrow
import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           PetaVision.Utility.HDF5
import           PetaVision.Utility.Parallel
import           System.Random

{-# INLINE generateData #-}

generateData :: Int -> Int -> Int -> IO (Int, R.Array U DIM3 Double)
generateData rows cols nf = do
  label <- randomRIO (0, 9) :: IO Int
  xs <- M.replicateM (rows * cols * nf) (randomRIO (-1, 1)) :: IO [Double]
  return (label, fromListUnboxed (Z :. rows :. cols :. nf) xs)

main = do
  let n = 10
      rows = 96
      cols = 96
      nf = 96
      parallelParams = ParallelParams 1 1 
  xs <- M.replicateM n (generateData rows cols nf)
  runResourceT $
    sourceList xs $$ CL.map (\(x, y) -> (fromIntegral x, [y])) =$=
    hdf5Sink parallelParams "SizeTest/HDF5"
  runResourceT $
    sourceList xs $$ CL.map (\x -> [x]) =$= saveFloatDataSink "SizeTest/" 40
