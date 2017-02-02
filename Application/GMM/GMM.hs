{-# LANGUAGE BangPatterns #-}

module Application.GMM.GMM where

import           Application.GMM.Gaussian
import           Application.GMM.MixtureModel
import           Control.DeepSeq                              as DS
import           Control.Monad                                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           PetaVision.Utility.Parallel
import           PetaVision.Utility.Time
import           Data.Array.Repa                              as R
import           Data.Binary
import qualified Data.ByteString.Lazy                         as BL
import           Data.Conduit
import           Data.Conduit.List                            as CL
import           Data.List                                    as L
import           Data.Maybe
import           Data.Vector                                  as V
import           Data.Vector.Unboxed                          as VU
import           Prelude                                      as P
import           System.IO                                    as IO
import           Text.Printf

type GMM = MixtureModel Gaussian

type AssignmentVec = [[Double]]

data ResetOption
  = ResetAll
  | ResetIndex ![Int]
  deriving (Show)

instance NFData ResetOption where
  rnf !_ = ()

data EMState a
  = EMDone !Double
           !a
  | EMContinue !AssignmentVec
               !Double
               !Int
               !a
  | EMReset !ResetOption
            !a
  deriving (Show)

instance NFData a =>
         NFData (EMState a) where
  rnf !_ = ()

initializeGMM :: Int -> [((Double, Double), (Double, Double))] -> IO GMM
initializeGMM numModel' bound = do
  gs <- M.replicateM numModel' (randomGaussian bound)
  initializeMixture numModel' gs

resetGMM :: ResetOption -> GMM -> [((Double, Double), (Double, Double))] -> IO GMM
resetGMM ResetAll gmm bound = initializeGMM (numModel gmm) bound
resetGMM (ResetIndex idx) (MixtureModel n models) bound = do
  gs <- V.replicateM (V.length vec) (randomGaussian bound)
  let !idxModels = V.zip vec gs
  return $!
    MixtureModel
      n
      (V.toList $
       V.generate
         n
         (\i ->
             let mi@(Model (wi, _)) = modelVec V.! i
             in case V.find (\(j, _) -> i == j) idxModels of
                  Nothing      -> mi
                  Just (_, gm) -> Model (wi, gm)))
  where
    !vec = V.fromList idx
    !modelVec = V.fromListN n models

getAssignment  :: GMM -> VU.Vector Double -> [Double]
getAssignment (MixtureModel _n models) x'
  -- | L.any isNaN ys = error $ show x' L.++ "\n" L.++ show ys L.++ "\n" L.++ show (L.map (\i -> models !! i) nanIndices)
  -- | isNaN s = error $ show nanIndices
  | s == 0 = L.replicate (L.length ys) 0
  | otherwise = L.map (\xx -> xx / s) ys
  where
    !ys =
      L.map
        (\(Model (weight, gaussianModel)) -> let z = weight * gaussian gaussianModel x'
                                             in if isNaN z
                                                   then 0
                                                   else z)
        models
    !s = L.sum ys
    nanIndices = L.findIndices isNaN ys

getAssignmentVec :: GMM -> [VU.Vector Double] -> AssignmentVec
getAssignmentVec gmm = L.map (getAssignment gmm)

getNks :: AssignmentVec -> [Double]
getNks = L.foldl1' (L.zipWith (+))

updateMu :: AssignmentVec
         -> [Double]
         -> [VU.Vector Double]
         -> [VU.Vector Double]
updateMu assignmentVec nks =
  L.zipWith (\nk x' -> VU.map (/ nk) x') nks .
  L.foldl1' (L.zipWith (VU.zipWith (+))) .
  L.zipWith (\assignment x' -> L.map (\y' -> VU.map (* y') x') assignment) assignmentVec

updateSigma
  :: AssignmentVec
  -> [Double]
  -> [VU.Vector Double]
  -> [VU.Vector Double]
  -> [VU.Vector Double]
updateSigma assignmentVec nks newMu =
  L.zipWith (\nk x' -> VU.map (/ nk) x') nks .
  L.foldl1' (L.zipWith (VU.zipWith (+))) .
  L.zipWith
    (\assignment x' ->
        L.zipWith
          (\a mu -> VU.map (* a) $ VU.zipWith (\y' m -> (y' - m) ^ (2 :: Int)) x' mu)
          assignment
          newMu)
    assignmentVec

updateW :: Int -> [Double] -> [Double]
updateW n w = L.map (/ L.sum vec) vec
  where
    !vec = L.map (/ fromIntegral n) w

updateGMM :: GMM -> AssignmentVec -> [VU.Vector Double] -> GMM
updateGMM oldGMM oldAssignmentVec xs =
  MixtureModel
    (numModel oldGMM)
    (L.zipWith3 (\w m s -> Model (w, Gaussian m s)) newW newMu newSigma)
  where
    !nks = getNks oldAssignmentVec
    !newMu = updateMu oldAssignmentVec nks xs
    !newSigma = updateSigma oldAssignmentVec nks newMu xs
    !newW = updateW (L.length xs) nks

getAvgLikelihood :: GMM -> [VU.Vector Double] -> Double
getAvgLikelihood gmm xs =
  L.foldl'
    (\ss x' ->
        ss +
        (log .
         L.foldl'
           (\s (Model (weight, gaussianModel)) ->
               s + weight * gaussian gaussianModel x')
           0 .
         model $
         gmm))
    0
    xs /
  fromIntegral (L.length xs)

em
  :: Double -> Double
  -> Int 
  -> GMM -> GMM
  -> [((Double, Double), (Double, Double))]
  -> [VU.Vector Double]
  -> IO GMM
em threshold lastAvgLikelihood count' oldGMM lastGMM bound xs
  | not (L.null smallVarianceIdx) =
    do putStrLn "reset small variance Gaussian"
       print smallVarianceIdx
       print . L.map (\x -> (snd . snd $ x) / 10000) $ bound
       gmm <- resetGMM (ResetIndex smallVarianceIdx) oldGMM bound
       em threshold lastAvgLikelihood (count' + 1) gmm lastGMM bound xs
  | not (L.null zeroNaNNKIdx) =
    do putStrLn "reset models which have no points assigned to them."
       print zeroNaNNKIdx
       print nks
       let ys = ((L.transpose oldAssignmentVec) !! (L.head zeroNaNNKIdx))
           is = L.findIndices (/= 0) ys
       print $ L.take 5 $ L.map (\i -> (ys !! i)) is
       gmm <- resetGMM (ResetIndex zeroNaNNKIdx) oldGMM bound
       em threshold lastAvgLikelihood 0 gmm lastGMM bound xs
  | not (L.null zeroZIdx) =
    do putStrLn "reset all"
       let x = (xs !! (fromJust zeroZIdx))
           y =
             L.map (\(Model (weight,gaussianModel)) ->
                      gaussian gaussianModel x)
                   (model oldGMM)
       print x
       print y
       gmm <- resetGMM ResetAll oldGMM bound
       em threshold lastAvgLikelihood 0 gmm lastGMM bound xs
  | oldAvgLikelihood < lastAvgLikelihood =
    do printCurrentTime
       printf "%0.2f    %0.2f\n" oldAvgLikelihood lastAvgLikelihood
       return lastGMM
  | rate < threshold || count' >= 50 =
    do printCurrentTime
       printf "%0.2f\n" oldAvgLikelihood
       return oldGMM
  | otherwise =
    do printCurrentTime
       printf "%0.2f\n" oldAvgLikelihood
       em threshold oldAvgLikelihood (count' + 1) newGMM oldGMM bound xs
  where oldAssignmentVec = getAssignmentVec oldGMM xs
        oldAvgLikelihood = getAvgLikelihood oldGMM xs
        nks = getNks oldAssignmentVec
        zs = L.map L.sum oldAssignmentVec
        zeroZIdx = L.elemIndex 0 zs
        zeroNaNNKIdx =
          L.findIndices (\x' -> x' == 0 || isNaN x')
                        nks
        newGMM = updateGMM oldGMM oldAssignmentVec xs
        -- !newAvgLikelihood = getAvgLikelihood newGMM xs
        rate = abs $ (lastAvgLikelihood - oldAvgLikelihood) / lastAvgLikelihood
        smallVarianceIdx =
          L.findIndices
            (\(Model (_,gm)) ->
               L.and .
               L.zipWith (\(_,(_,up)) x -> x < (up / 10000)) bound .
               VU.toList . gaussianSigma2 $
               gm) .
          model $
          oldGMM
-- em
--   :: Double
--   -> Int
--   -> GMM
--   -> [((Double, Double), (Double, Double))]
--   -> [VU.Vector Double]
--   -> IO GMM
-- em threshold count' oldGMM bound xs
--   | not (L.null zeroNaNNKIdx) = do
--     print zeroNaNNKIdx
--     gmm <- resetGMM (ResetIndex zeroNaNNKIdx) oldGMM bound
--     em threshold 0 gmm bound xs
--   -- | isJust zeroZIdx = do
--   --   printCurrentTime
--   --   putStrLn "Reset all"
--   --   let !x' = xs !! (fromJust zeroZIdx) 
--   --   print x'
--   --   print oldGMM
--   --   print $ L.map (\(Model (w,gm)) -> gaussian gm x') . model $ oldGMM
--   --   print $ getAssignment oldGMM x'
--   --   gmm <- resetGMM ResetAll oldGMM bound
--   --   em threshold 0 gmm bound xs
--   | rate < threshold || count' == 50 = do
--     printCurrentTime
--     printf "%0.2f\n" oldAvgLikelihood
--     return oldGMM
--   | otherwise = do
--     printCurrentTime
--     printf "%0.2f\n" oldAvgLikelihood
--     em threshold (count' + 1) newGMM bound xs
--   where
--     !oldAssignmentVec = getAssignmentVec oldGMM xs
--     !oldAvgLikelihood = getAvgLikelihood oldGMM xs
--     !nks = getNks oldAssignmentVec
--     !zs = L.map L.sum oldAssignmentVec
--     !zeroZIdx = L.elemIndex 0 zs
--     !zeroNaNNKIdx = L.findIndices (\x' -> x' == 0 || isNaN x') nks
--     !newGMM = updateGMM oldGMM oldAssignmentVec xs
--     !newAvgLikelihood = getAvgLikelihood newGMM xs
--     !rate = abs $ (oldAvgLikelihood - newAvgLikelihood) / oldAvgLikelihood

gmmSink
  :: ParallelParams
  -> FilePath -> FilePath
  -> Int
  -> Double
  -> Int
  -> Sink [VU.Vector Double] (ResourceT IO) ()
gmmSink parallelParams filePath muVarFilePath numM threshold numTrain = do
  xs <- CL.take numTrain
  let !ys = L.concatMap (L.map (VU.map (*100))) xs
      -- ys = L.concat xs
      bound = getFeatureBound parallelParams ys
      -- muVar = getFeatureMuVar parallelParams ys
      -- zs = normalizeFeature parallelParams muVar ys
  liftIO $ print bound
  gmm <- liftIO $ initializeGMM numM bound
  newGMM <- liftIO $ em threshold (fromIntegral (minBound::Int)) 0 gmm gmm bound ys
  liftIO $ writeGMM filePath [newGMM]
  -- liftIO $ encodeFile muVarFilePath (VU.toList muVar)

-- hGMMSink
--   :: ParallelParams
--   -> Handle
--   -> Int
--   -> Double
--   -> Int
--   -> Sink (Array U DIM3 Double) (ResourceT IO) [Array U DIM3 Double]
-- hGMMSink parallelParams handle numM  threshold numTrain = do
--   arrs <- CL.take numTrain
--   let !xs' = parMapChunk parallelParams rdeepseq extractPointwiseFeature arrs
--       !ys = L.concat xs'
--       !bound = getFeatureBound parallelParams ys
--   gmm <- liftIO $ initializeGMM numM bound
--   newGMM <- liftIO $ em threshold 0 gmm bound ys
--   liftIO $ hPutGMM handle newGMM
--   return arrs
  
-- hGMMSink1
--   :: ParallelParams -> Handle
--   -> Int
--   -> Double
--   -> Int
--   -> Sink [VU.Vector Double] (ResourceT IO) ()
-- hGMMSink1 parallelParams handle numM threshold numTrain = do
--   xs' <- CL.take numTrain
--   let !ys = L.concat xs'
--       !bound = getFeatureBound parallelParams ys
--   gmm <- liftIO $ initializeGMM numM bound
--   newGMM <- liftIO $ em threshold 0 gmm bound ys
--   liftIO $ hPutGMM handle newGMM

hPutGMM :: Handle -> GMM -> IO ()
hPutGMM handle gmm = do
  BL.hPut handle (encode len)
  BL.hPut handle encodedGMM
  where
    !encodedGMM = encode gmm
    !len = fromIntegral $ BL.length encodedGMM :: Word32

readGMM :: FilePath -> IO [GMM]
readGMM filePath = do gs <- withBinaryFile filePath ReadMode hGetGMM
                      if L.null gs
                         then error "readGMM: empty GMM file!"
                         else return gs
  where
    hGetGMM h = do
      sizebs <- BL.hGet h 4
      if BL.length sizebs < 4
        then return []
        else do
          let !size' = fromIntegral (decode sizebs :: Word32) :: Int
          bs <- BL.hGet h size'
          gmms <- hGetGMM h
          return $! decode bs : gmms

writeGMM :: FilePath -> [GMM] -> IO ()
writeGMM filePath gmms =
  withBinaryFile filePath WriteMode (\h -> M.mapM_ (hPutGMM h) gmms)

getFeatureBound :: ParallelParams
                -> [VU.Vector Double]
                -> [((Double, Double), (Double, Double))]
getFeatureBound parallelParams xs =
  parMapChunk
    parallelParams
    rdeepseq
    (\y' ->
        let m = L.sum y' / (fromIntegral . L.length $ y')
            d =
              (L.sum . L.map (^ (2 :: Int)) $ y') /
              (fromIntegral . L.length $ y')
            up = d - m ^ (2 :: Int)
        in ((L.minimum y' / 4, L.maximum y' / 4), (up / 2, up * 10)))
    ys
  where
    !ys = L.transpose . L.map VU.toList $ xs
    
getFeatureMuVar :: ParallelParams
                -> [VU.Vector Double]
                -> VU.Vector (Double,Double)
getFeatureMuVar parallelParams xs = VU.zip m sigma2
  where len = L.length xs
        s1 = L.foldl1' (VU.zipWith (+)) xs
        s2 = L.foldl1' (VU.zipWith (+)) . L.map (VU.map (^ (2 :: Int))) $ xs
        m = VU.map (/ fromIntegral len) s1
        sigma2 =
          VU.map sqrt $
          VU.zipWith (-)
                     (VU.map (/ fromIntegral len) s2)
                     (VU.map (^ (2 :: Int)) m)

normalizeFeature :: ParallelParams
                 -> VU.Vector (Double,Double)
                 -> [VU.Vector Double]
                 -> [VU.Vector Double]
normalizeFeature parallelParams muVar =
  parMapChunk
    parallelParams
    rdeepseq
    (VU.zipWith (\(mu,var) x -> (x - mu) / var)
                muVar)
