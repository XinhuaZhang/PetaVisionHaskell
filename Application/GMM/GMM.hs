{-# LANGUAGE BangPatterns #-}

module Application.GMM.GMM
  (GMM
  ,GMMData
  ,GMMParameters
  ,assignPoint
  ,assignGMM
  ,updateMuGMM
  ,updateSigmaGMM
  ,updateWGMM
  ,gmmTestSink
  ,gmmSink)
  where

import           Application.GMM.Gaussian
import           Application.GMM.MixtureModel
import           Control.DeepSeq              as DS
import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Parallel
import           CV.Utility.Parallel
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.Maybe
import           Data.Time
import           Data.Vector                  as V
import           Data.Vector.Unboxed          as VU
import           GHC.Generics
import           Prelude                      as P
import           System.Directory
import           System.Random
import           Text.Printf


type GMM = MixtureModel Gaussian

type GMMData = VU.Vector Double

type GMMParameters = VU.Vector Double

assignPoint
  :: Model Gaussian -> Double -> GMMData -> Double
assignPoint (Model (w,g)) z x = DS.force (w * gaussian g x) / z

assignGMM
  :: ParallelParams
  -> GMM
  -> V.Vector GMMData
  -> IO (V.Vector Double,V.Vector Double,Double, GMM) 
assignGMM parallelParams gmm@(MixtureModel n modelVec) xs
  | V.length smallVarIdx > 0 =
    do putStrLn "Variances of some Gaussians are too small. Overfitting could happen. Reset."
       print smallVarIdx
       newModel <- resetGMM gmm smallVarIdx
       assignGMM parallelParams newModel xs
  | isJust zeroZIdx =
    error $
    "There is one data point which is assigned to none of the model. Try to increase the initialization range of sigma and to decrease that of mu.\n" P.++
    (show (xs V.! fromJust zeroZIdx)) P.++
    "\n" P.++
    (show $ probability (xs V.! fromJust zeroZIdx))
  | V.length zeroKIdx > 0 =
    do putStrLn "There are models which have no point assigned to them! Reset them now."
       print zeroKIdx
       newModel <- resetGMM gmm zeroKIdx
       assignGMM parallelParams newModel xs
  | otherwise = return (zs,nks,likelihood,gmm)
  where !zs =
          parMapChunkVector
            parallelParams
            rdeepseq
            (\x ->
               V.foldl' (\s (Model (wj,mj)) -> s + (wj * gaussian mj x)) 0 $
               modelVec)
            xs
        nks =
          parMapChunkVector
            parallelParams
            rdeepseq
            (\m -> V.sum . V.zipWith (\z x -> assignPoint m z x) zs $ xs)
            modelVec
        likelihood = getLikelihood zs
        zeroZIdx = V.findIndex (== 0) zs
        zeroKIdx =
          V.findIndices (\x -> x == 0 || isNaN x)
                        nks
        smallVarIdx =
          V.findIndices
            (\(Model (w,Gaussian _ _ sigmaVec)) ->
               case VU.find (< 0.05) sigmaVec of
                 Nothing -> False
                 Just _ -> True)
            modelVec
        probability y =
                  V.map (\(Model (wj,mj)) -> (wj * gaussian mj y)) modelVec

resetGMM :: GMM -> V.Vector Int -> IO GMM
resetGMM gmm@(MixtureModel n modelVec) idx =
  do time <- liftIO getCurrentTime
     let gen =
           mkStdGen . P.fromIntegral . diffTimeToPicoseconds . utctDayTime $
           time
         models' =
           V.unfoldrN
             (V.length idx)
             (\g ->
                Just $
                randomGaussian
                  ((\(Model (_,gm)) -> numDims gm) $ V.head modelVec)
                  g)
             gen
         idxModels = V.zip idx models'
     return $!
       MixtureModel
         n
         (V.generate
            n
            (\i ->
               let mi@(Model (wi,_)) = modelVec V.! i
               in case V.find (\(j,_) -> i == j) idxModels of
                    Nothing -> mi
                    Just (_j,gm) -> Model (wi,gm)))


updateMuKGMM :: Model Gaussian
             -> V.Vector Double
             -> V.Vector GMMData
             -> Double
             -> GMMParameters
updateMuKGMM mg zs xs nk =
  VU.map (/ nk) .
  V.foldl1' (VU.zipWith (+)) .
  V.zipWith (\z x -> VU.map (* (assignPoint mg z x)) x) zs $
  xs

updateMuGMM :: ParallelParams
            -> GMM
            -> V.Vector Double
            -> V.Vector GMMData
            -> V.Vector Double
            -> V.Vector GMMParameters
updateMuGMM parallelParams gmm@(MixtureModel n modelVec) zs xs nks =
  parZipWithChunkVector parallelParams
                        rdeepseq
                        (\modelK nk -> updateMuKGMM modelK zs xs nk)
                        modelVec
                        nks

updateSigmaKGMM :: Model Gaussian
                -> V.Vector Double
                -> V.Vector GMMData
                -> Double
                -> GMMParameters
                -> GMMParameters
updateSigmaKGMM modelK zs xs nk newMuK
  | isJust smallIdx =
    VU.map (\x ->
              if x == 0
                 then 100
                 else x)
           newSigma
  | otherwise = newSigma
  where newSigma =
          VU.map (\x -> sqrt (x / nk)) .
          V.foldl1' (VU.zipWith (+)) .
          V.zipWith (\z x ->
                       VU.map (* (assignPoint modelK z x)) .
                       VU.zipWith (\mu y -> (y - mu) ^ 2)
                                  newMuK $
                       x)
                    zs $
          xs
        smallIdx = VU.findIndex (< 0.1) newSigma

updateSigmaGMM :: ParallelParams
               -> GMM
               -> V.Vector Double
               -> V.Vector GMMData
               -> V.Vector Double
               -> V.Vector GMMParameters
               -> V.Vector GMMParameters
updateSigmaGMM parallelParams gmm@(MixtureModel n modelVec) zs xs nks newMu =
  parZipWith3ChunkVector parallelParams
                         rdeepseq
                         (\modelK nk muK -> updateSigmaKGMM modelK zs xs nk muK)
                         modelVec
                         nks
                         newMu

updateWGMM
  :: Int -> V.Vector Double -> V.Vector Double
updateWGMM n = V.map (/ fromIntegral n)

getLikelihood :: V.Vector Double -> Double
getLikelihood = V.foldl' (\a b -> a + log b) 0

-- EM algorithm
emTest :: ParallelParams
       -> V.Vector GMMData
       -> Double
       -> GMM
       -> IO (GMM
             ,Double
             ,V.Vector Double
             ,V.Vector GMMParameters
             ,V.Vector GMMParameters
             ,V.Vector Double)
emTest parallelParams xs threshold oldModel = undefined
  -- | V.or . V.map (<= 0) $ nks =
  --   error "nk is zero! Try increasing the initialization range of sigma and decreasing that of mu."
  -- | isNaN likelihood =
  --   error "Likelihood is NaN! There must be something wrong."
  -- | otherwise = return (newModel,likelihood,nks,newMu,newSigma,newW)
  -- where (zs,nks,likelihood) = assignGMM parallelParams oldModel xs
  --       newMu = updateMuGMM parallelParams oldModel zs xs nks
  --       newSigma = updateSigmaGMM parallelParams oldModel zs xs nks newMu
  --       !newW =
  --         updateWGMM (V.length xs)
  --                    nks
  --       !newModel =
  --         newMu `pseq`
  --         newSigma `pseq`
  --         MixtureModel (numModel oldModel) $
  --         V.zipWith3
  --           (\w mu sigma ->
  --              Model (w
  --                    ,Gaussian (numDims .
  --                               snd . (\(Model x) -> x) . V.head . model $
  --                               oldModel)
  --                              mu
  --                              sigma))
  --           newW
  --           newMu
  --           newSigma

em :: ParallelParams
   -> FilePath
   -> V.Vector GMMData
   -> Double
   -> Double
   -> GMM
   -> IO ()
em parallelParams filePath xs threshold oldLikelihood oldModel =
  do (zs,nks,newLikelihood,intermediateModel) <-
       assignGMM parallelParams oldModel xs
     let newMu = updateMuGMM parallelParams intermediateModel zs xs nks
         newSigma =
           updateSigmaGMM parallelParams intermediateModel zs xs nks newMu
         !newW =
           updateWGMM (V.length xs)
                      nks
         !nD =
           numDims . snd . (\(Model x) -> x) . V.head . model $
           intermediateModel
         !newModel =
           newW `par`
           newMu `pseq`
           MixtureModel (numModel intermediateModel) $
           V.zipWith3 (\w mu sigma -> Model (w,Gaussian nD mu sigma))
                      newW
                      newMu
                      newSigma
         !avgLikelihood =
           log $
           (exp (newLikelihood / (P.fromIntegral $ V.length xs))) /
           ((2 * pi) ** (0.5 * (fromIntegral nD)))
     time <- liftIO getZonedTime
     let timeStr =
            (show . localTimeOfDay . zonedTimeToLocalTime $ time) P.++ ": "
     printf (timeStr P.++ "%0.2f (%0.3f%%)\n")
            avgLikelihood
            ((avgLikelihood - oldLikelihood) / (abs oldLikelihood) * 100)
     if avgLikelihood > threshold
        then liftIO $ encodeFile filePath intermediateModel
        else do liftIO $ encodeFile filePath intermediateModel
                em parallelParams filePath xs threshold avgLikelihood newModel
        

initializeGMM :: Int -> Int -> IO GMM
initializeGMM numModel numDimension =
  do time <- getCurrentTime
     let gen = mkStdGen . P.fromIntegral . diffTimeToPicoseconds . utctDayTime $ time
         (w',gen1) =
           randomRList numModel
                       (1,100)
                       gen
         ws' = P.sum $ w'
         w = V.fromList $ P.map (/ ws') w'
         models' =
           V.unfoldrN numModel
                      (\g -> Just $ randomGaussian numDimension g)
                      gen1
         models = V.zipWith (\a b -> Model (a,b)) w models'
     return (MixtureModel numModel models)

randomRList :: (RandomGen g,Random a)
            => Int -> (a,a) -> g -> ([a],g)
randomRList len bound gen
  | len > 0 =
    (\(xs,g) -> (x : xs,g)) $
    randomRList (len - 1)
                bound
                newGen
  | otherwise = ([],gen)
  where (x,newGen) = randomR bound gen

randomGaussian :: (RandomGen g)
               => Int -> g -> (Gaussian,g)
randomGaussian numDimension gen =
  (Gaussian numDimension
            (VU.fromList mu)
            (VU.fromList sigma)
  ,newGen2)
  where (mu,newGen1) =
          randomRList numDimension
                      (-5,5)
                      gen
        (sigma,newGen2) =
          randomRList numDimension
                      (1,100)
                      newGen1

gmmTestSink :: ParallelParams
            -> Int
            -> Double
            -> FilePath
            -> Sink (V.Vector GMMData) IO ()
gmmTestSink parallelParams numM threshold filePath =
  do xs <- consume
     models <-
       liftIO $
       initializeGMM numM
                     (VU.length . V.head . P.head $ xs)
     let !ys = V.concat xs
     trainedModel <-
       liftIO $
       M.foldM (\(oldLike,oldModel) b ->
                  do (newModel,like,nks,newMu,newSigma,newW) <-
                       emTest parallelParams ys threshold oldModel
                     putStrLn $ (show (b + 1)) P.++ ":"
                     putStrLn $
                       "likelihood: " P.++ (show like) P.++ " (" P.++
                       (show $ abs $ (like - oldLike) / oldLike * 100) P.++
                       "%)"
                     -- putStrLn $ "assignment: " P.++ show (V.take 5 assignment)
                     -- putStrLn $ "zs: " P.++ show (V.take 5 zs)
                     -- putStrLn $ "nks: " P.++ show (V.take 5 nks)
                     -- putStrLn $ "newMu: " P.++ show (V.head newMu)
                     -- putStrLn $ "newSigma: " P.++ show (V.head newSigma)
                     -- putStrLn $ "newW: " P.++ show (V.take 5 newW)
                     return (like,newModel))
               (0,models)
               (V.generate 12 id)
     liftIO $ encodeFile filePath trainedModel

gmmSink :: ParallelParams
        -> Int
        -> Double
        -> FilePath
        -> Sink (V.Vector GMMData) IO ()
gmmSink parallelParams numM threshold filePath =
  do xs <- consume
     fileFlag <- liftIO $ doesFileExist filePath
     fileSize <- liftIO $ getFileSize filePath
     models <-
       liftIO $
       if fileFlag && (fileSize > 0)
          then decodeFile filePath
          else initializeGMM numM
                             (VU.length . V.head . P.head $ xs)
     let !ys = V.concat xs
     liftIO $ em parallelParams filePath ys threshold 0 models
