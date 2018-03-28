import           Application.Caffe.ArgsParser      as AP
import           Application.PVP2LibLinear.Conduit (pvpLabelSource)
import           Classifier.LibLinear.Example      (readLabelFile)
import           Control.Monad                     as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa                   as R
import           Data.Conduit
import           Data.Conduit.List                 as CL
import           Data.List                         as L
import           Data.Vector.Unboxed               as VU
import           PetaVision.Data.Pooling
import           PetaVision.Data.Pooling
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.HDF5
import           PetaVision.Utility.Parallel       as PA
import           System.Environment
import           System.FilePath

pvpConduit :: ParallelParams
           -> Conduit PVPOutputData (ResourceT IO) (R.Array U DIM3 Double)
pvpConduit parallelParams = do
  xs <- CL.take (PA.batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\arr ->
                   let arr' = pvpOutputData2Array arr
                   in deepSeqArray arr' arr')
                xs
        sourceList ys
        pvpConduit parallelParams)

concatConduit
  :: ParallelParams
  -> Conduit [R.Array U DIM3 Double] (ResourceT IO) [R.Array U DIM3 Double]
concatConduit parallelParams = do
  xs <- CL.take (PA.batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\arrs ->
                   let vec = VU.concat . L.map toUnboxed $ arrs
                       len =
                         L.sum . L.map (L.product . listOfShape . extent) $ arrs
                       arr = fromUnboxed (Z :. 1 :. 1 :. len) vec
                   in deepSeqArray arr [arr])
                xs
        sourceList ys
        concatConduit parallelParams)
        
concatArrayConduit
  :: ParallelParams
  -> Conduit [R.Array U DIM3 Double] (ResourceT IO) [R.Array U DIM3 Double]
concatArrayConduit parallelParams = do
  xs <- CL.take (PA.batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rseq
                (\arrs ->
                   let arr = computeS . L.foldl1' (R.++) . L.map delay $ arrs
                   in deepSeqArray arr [arr])
                xs
        sourceList ys
        concatArrayConduit parallelParams)   

main = do
  args <- getArgs
  if L.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  print params
  let parallelParams =
        ParallelParams (AP.numThread params) (AP.batchSize params)
  labels <-
    if L.isSuffixOf ".pvp" . labelFile $ params
      then runResourceT $ (pvpLabelSource . labelFile $ params) $$ CL.consume
      else readLabelFile . labelFile $ params
  if poolingFlag params
    then if concatFlag params
           then runResourceT $
                (sequenceSources .
                 L.zipWith
                   (\ps x ->
                      pvpFileSource x =$=
                      poolArrayConduit parallelParams (poolingType params) ps)
                   (poolingSize params) .
                 pvpFile $
                 params) $$
                concatConduit parallelParams =$=
                mergeSource (sourceList labels) =$=
                hdf5Sink
                  parallelParams
                  ((folderName params) </> (modelName params) </> "Vector")
           else runResourceT $
                (sequenceSources .
                 L.zipWith
                   (\ps x ->
                      pvpFileSource x =$=
                      poolArrayConduit parallelParams (poolingType params) ps)
                   (poolingSize params) .
                 pvpFile $
                 params) =$=
                -- concatArrayConduit parallelParams $$
                mergeSource (sourceList labels) =$=
                hdf5Sink
                  parallelParams
                  ((folderName params) </> (modelName params) </> "Array")
    else if concatFlag params
           then runResourceT $
                (sequenceSources .
                 L.map (\x -> pvpFileSource x =$= pvpConduit parallelParams) .
                 pvpFile $
                 params) $$
                concatConduit parallelParams =$=
                mergeSource (sourceList labels) =$=
                hdf5Sink
                  parallelParams
                  ((folderName params) </> (modelName params) </> "Vector")
           else runResourceT $
                (sequenceSources .
                 L.map (\x -> pvpFileSource x =$= pvpConduit parallelParams) .
                 pvpFile $
                 params) =$=
                -- concatArrayConduit parallelParams $$
                mergeSource (sourceList labels) =$=
                hdf5Sink
                  parallelParams
                  ((folderName params) </> (modelName params) </> "Array")
