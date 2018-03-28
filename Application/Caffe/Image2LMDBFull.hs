import           Application.Caffe.ArgsParser as AP
import           Application.Caffe.LMDB
import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           PetaVision.Image.ImageIO
import           PetaVision.Utility.Parallel  as PA
import           System.Environment
import           System.FilePath

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

main = do
  args <- getArgs
  if L.null args
    then error "run with --help to see options."
    else return ()
  params <- parseArgs args
  print params
  pathList <- readImagePathList (L.head . pvpFile $ params)
  labelList <-
    (L.map (\x -> read x :: Int) . L.lines) <$> readFile (labelFile $ params)
  let parallelParams =
        ParallelParams (AP.numThread params) (AP.batchSize params)
  print . L.length $ pathList
  runResourceT $
    sourceList pathList $$ readImageConduit True =$= CL.map imageContent =$=
    mergeSource (sourceList labelList) =$=
    CL.map (\x -> [x]) =$=
    saveDataSink (folderName params) (PA.batchSize parallelParams)
