{-# LANGUAGE BangPatterns #-}

module Application.GMM.GMM
  ( GMM
  , AssignmentVec
  , getAssignmentVec
  , gmmSink
  , testSink
  , readGMM
  , writeGMM
  , initializeGMM
  ) where

import           Application.GMM.Gaussian
import           Application.GMM.MixtureModel
import           Control.Arrow
import           Control.DeepSeq              as DS
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Parallel       as MP
import           Control.Monad.Trans.Resource
import           Data.Binary
import           Data.ByteString.Lazy         as BL
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Maybe
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           PetaVision.Utility.Parallel
import           PetaVision.Utility.Time
import           Prelude                      as P
import           System.Directory
import           System.IO                    as IO
import           Text.Printf

data ResetOption
  = ResetAll
  | ResetIndex !(V.Vector Int)
  deriving (Show)

instance NFData ResetOption where
  rnf (ResetIndex x) = x `seq` ()
  rnf _              = ()

data EMState a
  = EMDone !Double
           !a
  | EMContinue Assignment
               Double
               Double
               !a
  | EMReset !ResetOption
            !a
  deriving (Show)

instance NFData a =>
         NFData (EMState a) where
  rnf (EMContinue x y z zz) = x `seq` y `seq` z `seq` zz `seq` ()
  rnf (EMReset x _y)     = x `seq` ()
  rnf _                  = ()

type GMM = MixtureModel Gaussian

type AssignmentVec = V.Vector (VU.Vector Double)

type Assignment = (VU.Vector Double, AssignmentVec)

initializeGMM :: Int -> ((Double, Double), (Double, Double)) -> IO GMM
initializeGMM numModel' bound = do
  gs <- V.replicateM numModel' (randomGaussian bound)
  initializeMixture numModel' gs

resetGMM :: ResetOption -> GMM -> ((Double, Double), (Double, Double)) -> IO GMM
resetGMM ResetAll gmm bound = initializeGMM (numModel gmm) bound
resetGMM (ResetIndex vec) (MixtureModel n modelVec) bound = do
  gs <- V.replicateM (V.length vec) (randomGaussian bound)
  let !idxModels = V.zip vec gs
  return $!
    MixtureModel
      n
      (V.generate
         n
         (\i ->
             let mi@(Model (wi, _)) = modelVec V.! i
             in case V.find (\(j, _) -> i == j) idxModels of
                  Nothing      -> mi
                  Just (_, gm) -> Model (wi, gm)))

resetGMMList :: ((Double, Double), (Double, Double))
             -> [EMState GMM]
             -> IO [EMState GMM]
resetGMMList bound = P.mapM reset
  where
    reset (EMReset option gmm) = do
      newGMM <- resetGMM option gmm bound
      return $! EMContinue undefined undefined undefined newGMM
    reset gmmState = return gmmState

{-# INLINE getAssignment #-}

getAssignment :: GMM -> Double -> VU.Vector Double
getAssignment (MixtureModel _n modelVec) x = VU.map (/ s) vec
  where
    !vec =
      V.convert .
      V.map
        (\(Model (weight, gaussianModel)) -> weight * gaussian gaussianModel x) $
      modelVec
    !s = VU.sum vec

getAssignmentVec :: GMM -> VU.Vector Double -> Assignment
getAssignmentVec gmm xs =
  (getAssignment gmm) *** (V.map (getAssignment gmm) . VU.convert) $ (0, xs)

getNks :: Int -> (VU.Vector Double, AssignmentVec) -> VU.Vector Double
getNks n (assignment0, assignment) =
  VU.zipWith
    (+)
    (V.foldl1' (VU.zipWith (+)) assignment)
    (VU.map (* fromIntegral n) assignment0)

getAvgLikelihood :: GMM -> (Int, VU.Vector Double) -> Double
getAvgLikelihood gmm (n, xs) =
  (likelihood0 +
   VU.foldl'
     (\ss x ->
         ss +
         (log .
          V.foldl'
            (\s (Model (weight, gaussianModel)) ->
                s + weight * gaussian gaussianModel x)
            0 .
          model $
          gmm))
     0
     xs) /
  fromIntegral (VU.length xs + n)
  where
    likelihood0 =
      (fromIntegral n) *
      (log .
       V.foldl'
         (\s (Model (weight, gaussianModel)) ->
             s + weight * gaussian gaussianModel 0)
         0 .
       model $
       gmm)

updateMu :: Assignment -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double
updateMu (_assignment0, assignmentVec) nks =
  VU.zipWith (flip (/)) nks .
  V.foldl1' (VU.zipWith (+)) .
  V.zipWith (\assignment x -> VU.map (* x) assignment) assignmentVec . VU.convert

updateSigma
  :: Assignment
  -> VU.Vector Double
  -> VU.Vector Double
  -> VU.Vector Double
  -> VU.Vector Double
updateSigma (assignment0, assignmentVec) nks newMu =
  VU.zipWith (flip (/)) nks .
  VU.zipWith (+) (VU.zipWith (\a mu -> a * mu ^ (2 :: Int)) assignment0 newMu) .
  V.foldl1' (VU.zipWith (+)) .
  V.zipWith
    (\assignment x ->
        VU.zipWith (\a mu -> a * (x - mu) ^ (2 :: Int)) assignment newMu)
    assignmentVec .
  VU.convert

updateW :: Int -> VU.Vector Double -> VU.Vector Double
updateW n w = VU.map (/ VU.sum vec) vec
  where
    !vec = VU.map (/ fromIntegral n) w

emOneStep :: Double -> EMState GMM -> (Int, VU.Vector Double) -> EMState GMM
emOneStep _ x@(EMDone _ _) _ = x
emOneStep threshold (EMContinue oldAssignmentVec oldAvgLikelihood _ oldGMM) (n, xs)
  | not (V.null zeroNaNNKIdx) = EMReset (ResetIndex zeroNaNNKIdx) oldGMM
  | isJust zeroZIdx = EMReset ResetAll oldGMM
  | newAvgLikelihood > threshold ||
      abs ((newAvgLikelihood - oldAvgLikelihood) / oldAvgLikelihood) < 0.01 =
    EMDone newAvgLikelihood newGMM
  | otherwise = EMContinue newAssignmentVec newAvgLikelihood newRate newGMM
  where
    !nks = getNks n oldAssignmentVec
    !newMu = updateMu oldAssignmentVec nks xs
    !newSigma = updateSigma oldAssignmentVec nks newMu xs
    !newW = updateW (VU.length xs) nks
    !zs = V.map VU.sum . snd $ oldAssignmentVec
    !zeroZIdx = V.findIndex (\x -> x == 0 || isNaN x) zs
    !zeroNaNNKIdx = VU.convert $ VU.findIndices (\x -> x == 0 || isNaN x) nks
    !newGMM =
      MixtureModel
        (numModel oldGMM)
        (V.zipWith3
           (\w mu sigma -> Model (w, Gaussian mu sigma))
           (VU.convert newW)
           (VU.convert newMu)
           (VU.convert newSigma))
    !newAssignmentVec = getAssignmentVec newGMM xs
    !newAvgLikelihood = getAvgLikelihood newGMM (n, xs)
    !newRate = abs ((newAvgLikelihood - oldAvgLikelihood) / oldAvgLikelihood)
emOneStep _ (EMReset _ _) _ =
  error "emOneStep: There models needed to be reset!"

em
  :: Handle
  -> ((Double, Double), (Double, Double))
  -> Double
  -> [EMState GMM]
  -> [(Int, VU.Vector Double)]
  -> IO Handle
em handle bound threshold gmms xs =
  if P.all checkStateDone gmms
    then do
      let !avgLikelihood =
            (P.sum . P.map getStateLikelihood $ gmms) /
            fromIntegral (P.length gmms)
      printCurrentTime
      print . P.map getStateLikelihood' $ gmms
      printf "%0.2f\n" avgLikelihood
      hPutGMM handle (P.map getModelDone gmms)
      return handle
    else do
      printCurrentTime
      gmms1 <- resetGMMList bound gmms
      let !gmms2 = parZipWith rdeepseq computeStateAssignmentLikelihood gmms1 xs
          !newGMMs = parZipWith rdeepseq (emOneStep threshold) gmms2 xs
          !avgLikelihood =
            (P.sum . P.map getStateLikelihood $ gmms2) /
            fromIntegral (P.length gmms2)
      if isNaN avgLikelihood
        then IO.putStrLn "Reset"
        else do
          printf "%0.2f\n" avgLikelihood
          print . P.map getStateLikelihood' $ newGMMs
      em handle bound threshold newGMMs xs
  where
    checkStateDone EMDone {} = True
    checkStateDone _ = False
    computeStateAssignmentLikelihood (EMContinue _ _ _ m) (n, x) =
      let !assignment = getAssignmentVec m x
          !avgLikelihood = getAvgLikelihood m (n, x)
      in EMContinue assignment avgLikelihood (-1) m
    computeStateAssignmentLikelihood EMReset {} _ =
      error
        "computeStateAssignment: All reset state shold have been removed by now."
    computeStateAssignmentLikelihood state _ = state
    getStateLikelihood (EMContinue _ x _ _) = x
    getStateLikelihood (EMDone x _) = x
    getStateLikelihood _ =
      error
        "getStateLikelihood: All reset state shold have been removed by now."
    getStateLikelihood' (EMContinue _ x rate _) = ("EMContinue", x, rate)
    getStateLikelihood' (EMDone x _) = ("EMDone", x, -1)
    getStateLikelihood' (EMReset x (MixtureModel _ modelVec)) =
      case x of
        (ResetIndex vec) ->
          if V.length vec == 1
            then ("EMReset " P.++ show x P.++ " " P.++ show (modelVec V.! (V.head vec)), 0, -1)
            else ("EMReset " P.++ show x, 0, -1)
        _ -> ("EMReset " P.++ show x, 0, -1)
    getModelDone (EMDone _ m) = m
    getModelDone _ =
      error "getModelDone: There are states which are not done yet."

gmmSink
  :: ParallelParams
  -> FilePath
  -> Int
  -> Int
  -> ((Double, Double), (Double, Double))
  -> Double
  -> Sink (Int, VU.Vector Double) (ResourceT IO) ()
gmmSink parallelParams filePath numM numFeature bound threshold = do
  fileFlag <- liftIO $ doesFileExist filePath
  models <-
    liftIO $
    if fileFlag
      then do
        fileSize <- liftIO $ getFileSize filePath
        if fileSize > 0
          then do
            IO.putStrLn $ "Read GMM data file: " P.++ filePath
            readGMM filePath
          else M.replicateM numFeature $ initializeGMM numM bound
      else M.replicateM numFeature $ initializeGMM numM bound
  handle <- liftIO $ openBinaryFile filePath WriteMode
  liftIO $ BL.hPut handle (encode (fromIntegral numFeature :: Word32))
  go handle models
  liftIO $ hClose handle
  where
    go h gmms = do
      xs <- CL.take (batchSize parallelParams)
      unless
        (L.null xs)
        (do let !(as, bs) = L.splitAt (L.length xs) gmms
                !stateGMM =
                  parZipWith
                    rdeepseq
                    (\gmm (n, ys) ->
                        let !assignment = getAssignmentVec gmm ys
                            !likelihood = getAvgLikelihood gmm (n, ys)
                        in EMContinue assignment likelihood (1/0) gmm)
                    as
                    xs
            h' <- liftIO $ em h bound threshold stateGMM xs
            go h' bs)

hPutGMM :: Handle -> [GMM] -> IO ()
hPutGMM handle =
  M.mapM_
    (\x -> do
       let y = encode x
           len = P.fromIntegral $ BL.length y :: Word32
       BL.hPut handle (encode len)
       BL.hPut handle y)

readGMM :: FilePath -> IO [GMM]
readGMM filePath =
  withBinaryFile
    filePath
    ReadMode
    (\h -> do
       lenbs <- hGet h 4
       let len = fromIntegral (decode lenbs :: Word32) :: Int
       M.replicateM len (hGetGMM h))
  where
    hGetGMM h = do
      sizebs <- BL.hGet h 4
      let size = fromIntegral (decode sizebs :: Word32) :: Int
      bs <- BL.hGet h size
      return $ decode bs

writeGMM :: FilePath -> [GMM] -> IO ()
writeGMM filePath gmms =
  withBinaryFile
    filePath
    WriteMode
    (\h -> do
       BL.hPut h (encode (fromIntegral $ P.length gmms :: Word32))
       hPutGMM h gmms)


testSink :: Sink (Int,VU.Vector Double) (ResourceT IO) ()
testSink = do xs <- CL.take 1
              let (nz,vec) = P.head xs
              liftIO . print $ nz
              liftIO . print . VU.length $ vec
              liftIO . print . VU.maximum $ vec
