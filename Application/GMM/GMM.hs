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
  ,gmmSink)
  where

import           Application.GMM.Gaussian
import           Application.GMM.MixtureModel
import           Application.GMM.Representation
import           Control.DeepSeq                as DS
import           Control.Monad                  as M
import           Control.Monad.IO.Class
import           Control.Parallel
import           Data.Binary
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.Maybe
import           Data.Time
import           Data.Vector                    as V
import           Data.Vector.Unboxed            as VU
import           GHC.Generics
import           PetaVision.Utility.Parallel
import           Prelude                        as P
import           System.Directory
import           System.Random
import           Text.Printf


type GMM = MixtureModel Gaussian

type GMMData =  DataVec Double

type GMMParameters = DataVec Double

assignPoint
  :: Model Gaussian -> Double -> GMMData -> Double
assignPoint (Model (w,g)) z x = (w * gaussian g x) / z

assignGMM
  :: ParallelParams
  -> GMM
  -> V.Vector GMMData
  -> IO (V.Vector Double,V.Vector Double,Double, GMM)
assignGMM parallelParams gmm@(MixtureModel n modelVec) xs
  | V.length smallVarIdx > 0 =
    do putStrLn "Variances of some Gaussians are too small. Overfitting could happen. Reset."
       print $ P.length smallVarIdx
       print smallVarIdx
       print $ modelVec V.! (V.head smallVarIdx)
       newModel <- resetGMM gmm smallVarIdx
       assignGMM parallelParams newModel xs
  | V.length nanZIdx > 0 = error "Nan found! The variance is so small that the Gaussian probability equls to infinity * 0 = nan."
  | V.length zeroZIdx > 0 =
    do -- let x = xs V.! fromJust zeroZIdx
       --     (Model (_,(Gaussian _ mu' sigma'))) = V.head modelVec
       --     a = mu' - x
       --     b = powVec 2 (a / sigma')
       --     c = exp (-0.5 * sumVec b)
       --     d = productVec sigma'
       --     e = (2 * pi) ** (0.5 * (P.fromIntegral $ lengthVec sigma'))
       -- print mu'
       -- print sigma'
       -- print x
       -- print a
       -- print b
       -- print $ sumVec b
       -- print c
       -- print d
       -- print $ c / d / e
       print $ V.length zeroZIdx
       error $
         "There is one data point which is assigned to none of the model. Try to increase the initialization range of sigma and to decrease that of mu.\n" 
       -- P.++
       --   (show (xs V.! fromJust zeroZIdx)) P.++
       --   "\n" P.++
       --   (show $ probability (xs V.! fromJust zeroZIdx))
  | V.length zeroKIdx > 0 =
    do putStrLn "There are models which have no point assigned to them! Reset them now."
       print zeroKIdx
       newModel <- resetGMM gmm zeroKIdx
       assignGMM parallelParams newModel xs
  | otherwise = do return (zs,nks,likelihood,gmm)
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
        nanZIdx = V.findIndices isNaN zs
        zeroZIdx = V.findIndices (\x -> x == 0 ) zs
        zeroKIdx =
          V.findIndices (\x -> x == 0 )
                        nks
        smallVarIdx =
          V.findIndices
            (\(Model (w,Gaussian _ _ sigmaVec)) ->
               case findVec (< 0.02) sigmaVec of
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
                    Nothing      -> mi
                    Just (_j,gm) -> Model (wi,gm)))


updateMuKGMM :: Model Gaussian
             -> V.Vector Double
             -> V.Vector GMMData
             -> Double
             -> GMMParameters
updateMuKGMM mg zs xs nk =
  scalarMulVec (1 / nk) .
  addFoldVec .
  V.zipWith (\z x ->
               scalarMulVec (assignPoint mg z x)
                            x)
            zs $
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
updateSigmaKGMM modelK zs xs nk newMuK =
  powVec 0.5 .
  scalarMulVec (1 / nk) .
  addFoldVec .
  V.zipWith (\z x ->
--               scalarMulVec (assignPoint modelK z x) . powVec 2 $ (newMuK - x))
               scalarMulVec (assignPoint modelK z x) . powVec 2 $ x) -- assuming mu is zero
            zs $
  xs
          

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
     let --newMu = updateMuGMM parallelParams intermediateModel zs xs nks
         newMu = (\(MixtureModel _ vec) -> V.map (\(Model (_,gm)) -> mu gm) vec) intermediateModel 
         newSigma =
           updateSigmaGMM parallelParams intermediateModel zs xs nks newMu
         !newW =
           updateWGMM (V.length xs)
                      nks
         !nD =
           numDims . snd . (\(Model x) -> x) . V.head . model $
           intermediateModel
         !newModel =
           MixtureModel (numModel intermediateModel) $
           V.zipWith3 (\w mu sigma -> Model (w,Gaussian nD mu sigma))
                      newW
                      newMu
                      newSigma
         !avgLikelihood = -- newLikelihood / (P.fromIntegral $ V.length xs)
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
                       (1,1000)
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
            (fromListDense mu)
            (fromListDense sigma)
  ,newGen2)
  where (mu,newGen1) =
          randomRList numDimension
                      (0,0)
                      gen
        (sigma,newGen2) =
          randomRList numDimension
                      (1,10)
                      newGen1

gmmSink :: ParallelParams
        -> Int
        -> Double
        -> FilePath
        -> Sink (V.Vector GMMData) IO ()
gmmSink parallelParams numM threshold filePath =
  do xs <- CL.take 100
     fileFlag <- liftIO $ doesFileExist filePath
     model1 <-
       liftIO $
       if fileFlag
          then do fileSize <- liftIO $ getFileSize filePath
                  if (fileSize > 0)
                     then do putStrLn "Read GMM model from file."
                             decodeFile filePath
                     else initializeGMM numM
                                        (lengthVec . V.head . P.head $ xs)
          else initializeGMM numM
                             (lengthVec . V.head . P.head $ xs)
     model2 <-
       if (lengthVec . V.head . P.head $ xs) /=
          ((\(Model (_,gm)) -> numDims gm) . V.head . model $ model1)
          then liftIO $
               initializeGMM numM
                             (lengthVec . V.head . P.head $ xs)
          else return model1
     let !ys = V.concat xs
     liftIO $ em parallelParams filePath ys threshold 0 model2
