{-# LANGUAGE BangPatterns #-}

module Application.PVP2LibLinear.Conduit
  ( trainSink
  , concatConduit
  , predictConduit
  , concatPooledConduit
  , pvpLabelSource
  , vote
  , vote1
  ) where

import           Classifier.LibLinear.Bindings
import           Classifier.LibLinear.Example
import           Classifier.LibLinear.Interface
import           Control.Monad                  as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Maybe
import           Data.Vector.Unboxed            as VU
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel
import           Prelude                        as P
import           System.FilePath
import           Text.Printf

-- Assume that it is 1x1xnf
readPVPLabelFile :: FilePath -> IO [Double]
readPVPLabelFile filePath = do
  labels <- runResourceT $ pvpFileSource filePath $$ CL.consume
  return .
    P.map
      (\x ->
          case x of
            PVP_OUTPUT_ACT_SPARSEVALUES _ ys -> fromIntegral . fst . VU.head $ ys
            PVP_OUTPUT_NONSPIKING_ACT _ vec ->
              case VU.findIndex (/= 0) vec of
                Nothing -> error "pvpLabelSource: one label frame is all-zero."
                Just y -> fromIntegral y
            _ -> error "readPVPLabelFile: file format is supported.") $
    labels

pvpLabelSource :: FilePath -> Source (ResourceT IO) Double
pvpLabelSource filePath =
  pvpFileSource filePath =$=
  CL.map
    (\x ->
       case x of
         PVP_OUTPUT_ACT_SPARSEVALUES _ ys -> fromIntegral . fst . VU.head $ ys
         PVP_OUTPUT_NONSPIKING_ACT _ vec ->
           case VU.findIndex (/= 0) vec of
             Nothing -> error "pvpLabelSource: one label frame is all-zero."
             Just y -> fromIntegral y
         _ -> error "readPVPLabelFile: file format is supported.")

getFeaturePtr :: [(Int, Double)] -> IO (Ptr C'feature_node)
getFeaturePtr xs = newArray (pairs P.++ [C'feature_node (-1) 0])
  where
    pairs =
      P.map (\(i, x) -> C'feature_node (P.fromIntegral i) (realToFrac x)) xs

trainSink :: TrainParams
          -> [FilePath]
          -> Int -> Bool
          -> Sink (VU.Vector (Int, Double)) (ResourceT IO) ()
trainSink (TrainParams solver c numExample _ modelName) filePath batchSize findCFlag = do
  label <-
    liftIO .
    fmap (L.concat . L.transpose) .
    M.mapM
      (if L.isSuffixOf ".pvp" . L.head $ filePath
         then readPVPLabelFile
         else readLabelFile) $
    filePath
  (m,featurePtr) <- go (-1) []
  let params =
        TrainParams
        { trainSolver = solver
        , trainC = c
        , trainNumExamples = min numExample . L.length $ label
        , trainFeatureIndexMax = m
        , trainModel = modelName
        }
  liftIO $ print params
  liftIO $
    if findCFlag
      then findParameterC params label (L.take (L.length label) . L.concat . L.reverse $ featurePtr)
      else train params label (L.take (L.length label) . L.concat . L.reverse $ featurePtr)
  where
    go !maxIndex !ptrs = do
      xs <- CL.take batchSize
      if not (P.null xs)
        then do
          let !norm =
                P.map
                  (sqrt .
                   VU.foldl' (\a b -> a + b ^ (2 :: Int)) 0 . snd . VU.unzip)
                  xs
              ys = P.zipWith (\x n -> VU.map (\(i, v) -> (i, v / n)) x) xs norm
              zs = L.map (fst . VU.last) . L.filter (not . VU.null) $ xs
              mIdx = if L.null zs
                        then -1
                        else L.maximum zs
          featurePtr <- liftIO $ P.mapM (getFeaturePtr . VU.toList) ys
          go (max maxIndex mIdx) $ featurePtr : ptrs
        else return (maxIndex, ptrs)

-- liblinear feature node's index starts from 1 !!!!!
concatConduit :: [Int] -> Conduit [PVPOutputData] (ResourceT IO) (VU.Vector (Int, Double))
concatConduit offset =
  awaitForever
    (yield .
     VU.concat .
     P.zipWith (\i x -> VU.map (\(ind, val) -> (ind + i + 1, val)) x) offset .
     P.map parsePVPOutputData)
  -- awaitForever
  --   (\x -> let y = VU.concat .
  --                  P.zipWith (\i x -> VU.map (\(ind, val) -> (ind + i + 1, val)) x) offset .
  --                  P.map parsePVPOutputData $ x
  --              z = (\(PVP_OUTPUT_ACT_SPARSEVALUES dim xs) -> xs) . L.head $ x
  --          in do liftIO $ print . VU.length $ y
  --                liftIO $ print $ z
  --                yield y
  --    )
  where
    parsePVPOutputData :: PVPOutputData -> VU.Vector (Int, Double)
    parsePVPOutputData (PVP_OUTPUT_NONSPIKING_ACT _ xs) =
      VU.filter (\(_, v) -> v /= 0) $ VU.imap (\i v -> (i, v)) xs
    parsePVPOutputData (PVP_OUTPUT_ACT_SPARSEVALUES _ xs) = xs
    parsePVPOutputData _ = error "Doesn't support this type of pvpfile."

concatPooledConduit :: Conduit [VU.Vector (Int, Double)] (ResourceT IO) (VU.Vector (Int, Double))
concatPooledConduit = awaitForever (yield . VU.concat)

predictConduit :: Conduit (VU.Vector (Int, Double)) (ResourceT IO) (Ptr C'feature_node)
predictConduit =
  awaitForever
    (\x -> do
       let !norm =
             sqrt . VU.foldl' (\a b -> a + b ^ (2 :: Int)) 0 . snd . VU.unzip $
             x
           y = VU.map (\(i, v) -> (i, v / norm)) x
       ptr <- liftIO $ (getFeaturePtr . VU.toList) y
       yield ptr)


vote :: FilePath -> (VU.Vector Int, [((Int,Int), [Double])]) -> IO ()
vote timeStampFilePath (labels, prediction) = do
  ys <- readFile timeStampFilePath
  let fileNames = L.map takeBaseName . L.lines $ ys
      zs =
        L.groupBy (\a b -> snd a == snd b) . L.sortOn snd $
        L.zip prediction fileNames
      results =
        parMap
          rdeepseq
          (\pairList ->
             let target = fst . fst . fst . L.head $ pairList
                 labelIdx =
                   VU.maxIndex .
                   L.foldl1' (VU.zipWith (+)) . L.map (VU.fromList . snd . fst) $
                   pairList
             in if target == labels VU.! labelIdx
                  then 1 :: Int
                  else 0)
          zs
  M.mapM_ print . L.take 100 . L.zip results $ zs
  printf
    "%f%%\n"
    (100 * (fromIntegral . L.sum $ results :: Double) /
     (fromIntegral . L.length $ results))



vote1 :: FilePath -> (VU.Vector Int, [((Int,Int), [Double])]) -> IO ()
vote1 timeStampFilePath (labels, prediction) = do
  ys <- readFile timeStampFilePath
  let fileNames = L.map takeBaseName . L.lines $ ys
      zs =
        L.groupBy (\a b -> snd a == snd b) . L.sortOn snd $
        L.zip prediction fileNames
      results =
        parMap
          rdeepseq
          (\pairList ->
             let target = fst . fst . fst . L.head $ pairList
                 as =
                   L.map
                     (\xx ->
                        if xx == target
                          then 1 :: Int
                          else 0) .
                   snd . L.unzip . fst . L.unzip . fst . L.unzip $
                   pairList :: [Int]
             in if L.sum as > (div (L.length as) 2)
                  then 1 :: Int
                  else 0)
          zs
  -- M.mapM_ print . L.take 100 . L.zip results $ zs
  printf
    "%f%%\n"
    (100 * (fromIntegral . L.sum $ results :: Double) /
     (fromIntegral . L.length $ results))
