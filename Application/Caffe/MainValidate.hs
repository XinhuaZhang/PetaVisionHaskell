import           Application.Caffe.ArgsParser      as AP
import           Application.PVP2LibLinear.Conduit (pvpLabelSource)
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
                       arr = fromUnboxed (Z :. len :. 1 :. 1) vec
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

takeConduit :: Int -> Conduit a (ResourceT IO) a
takeConduit n = loop 0
  where
    loop m = do
      x <- await
      case x of
        Nothing -> return ()
        Just y ->
          if m == n
            then return ()
            else do
              yield y
              loop (m + 1)

dropConduit :: Int -> Conduit a (ResourceT IO) a
dropConduit n = do
  CL.drop n
  awaitForever yield

lenSink :: Int -> Sink a (ResourceT IO) Int
lenSink n = do
  x <- await
  case x of
    Nothing -> return n
    Just _  -> lenSink (n + 1)

main = do
  args <- getArgs
  if L.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  print params
  len' <- runResourceT $ pvpFileSource (L.head . pvpFile $ params) $$ lenSink 0
  print len'
  len'' <- runResourceT $ (pvpLabelSource . labelFile $ params) $$ lenSink 0
  print len''
  print . L.head . pvpFile $ params
  header <- readPVPHeader (L.head . pvpFile $ params)
  print header
  let parallelParams =
        ParallelParams (AP.numThread params) (AP.batchSize params)
      n = 0 -- 15000
      len = round $ (fromIntegral (len' - n)) * 4 / 5
  print len
  if poolingFlag params
    then if concatFlag params
           then runResourceT $
                (sequenceSources .
                 L.zipWith
                   (\ps x ->
                      pvpFileSource x =$= dropConduit n =$= takeConduit len =$=
                      poolArrayConduit parallelParams (poolingType params) ps)
                   (poolingSize params) .
                 pvpFile $
                 params) $$
                concatConduit parallelParams =$=
                mergeSource
                  ((pvpLabelSource . labelFile $ params) =$= dropConduit n) =$=
                hdf5Sink
                  parallelParams
                  ((folderName params) </> "Train" </> "Vector")
           else runResourceT $
                (sequenceSources .
                 L.zipWith
                   (\ps x ->
                      pvpFileSource x =$= takeConduit len =$=
                      poolArrayConduit parallelParams (poolingType params) ps)
                   (poolingSize params) .
                 pvpFile $
                 params) =$=
                concatArrayConduit parallelParams $$
                mergeSource (pvpLabelSource . labelFile $ params) =$=
                hdf5Sink
                  parallelParams
                  ((folderName params) </> "Train" </> "Array")
    else if concatFlag params
           then runResourceT $
                (sequenceSources .
                 L.map
                   (\x ->
                      pvpFileSource x =$= takeConduit len =$=
                      pvpConduit parallelParams) .
                 pvpFile $
                 params) $$
                concatConduit parallelParams =$=
                mergeSource (pvpLabelSource . labelFile $ params) =$=
                hdf5Sink
                  parallelParams
                  ((folderName params) </> "Train" </> "Vector")
           else runResourceT $
                (sequenceSources .
                 L.map
                   (\x ->
                      pvpFileSource x =$= takeConduit len =$=
                      pvpConduit parallelParams) .
                 pvpFile $
                 params) =$=
                concatArrayConduit parallelParams $$
                mergeSource (pvpLabelSource . labelFile $ params) =$=
                hdf5Sink
                  parallelParams
                  ((folderName params) </> "Train" </> "Array")
  if poolingFlag params
    then if concatFlag params
           then runResourceT $
                (sequenceSources .
                 L.zipWith
                   (\ps x ->
                      pvpFileSource x =$= dropConduit (len + n) =$=
                      poolArrayConduit parallelParams (poolingType params) ps)
                   (poolingSize params) .
                 pvpFile $
                 params) $$
                concatConduit parallelParams =$=
                mergeSource
                  ((pvpLabelSource . labelFile $ params) =$=
                   dropConduit (len + n)) =$=
                hdf5Sink
                  parallelParams
                  ((folderName params) </> "Validate" </> "Vector")
           else runResourceT $
                (sequenceSources .
                 L.zipWith
                   (\ps x ->
                      pvpFileSource x =$= dropConduit len =$=
                      poolArrayConduit parallelParams (poolingType params) ps)
                   (poolingSize params) .
                 pvpFile $
                 params) =$=
                concatArrayConduit parallelParams $$
                mergeSource
                  ((pvpLabelSource . labelFile $ params) =$= dropConduit len) =$=
                hdf5Sink
                  parallelParams
                  ((folderName params) </> "Validate" </> "Array")
    else if concatFlag params
           then runResourceT $
                (sequenceSources .
                 L.map
                   (\x ->
                      pvpFileSource x =$= dropConduit len =$=
                      pvpConduit parallelParams) .
                 pvpFile $
                 params) $$
                concatConduit parallelParams =$=
                mergeSource
                  ((pvpLabelSource . labelFile $ params) =$= dropConduit len) =$=
                hdf5Sink
                  parallelParams
                  ((folderName params) </> "Validate" </> "Vector")
           else runResourceT $
                (sequenceSources .
                 L.map
                   (\x ->
                      pvpFileSource x =$= dropConduit len =$=
                      pvpConduit parallelParams) .
                 pvpFile $
                 params) =$=
                concatArrayConduit parallelParams $$
                mergeSource
                  ((pvpLabelSource . labelFile $ params) =$= dropConduit len) =$=
                hdf5Sink
                  parallelParams
                  ((folderName params) </> "Validate" </> "Array")
