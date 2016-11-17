{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeOperators #-}
module Application.GMM.FisherKernel where

import           Application.GMM.Gaussian
import           Application.GMM.GMM
import           Application.GMM.MixtureModel
import           Application.GMM.Representation
import           Control.DeepSeq                as DS
import           Control.Monad.IO.Class
import           Data.Binary
import           Data.ByteString.Lazy           as BL
import           Data.Conduit
import           Data.Conduit.List              as CL
import           Data.List                      as L
import           Data.Maybe
import           Data.Vector                    as V
import           Data.Vector.Unboxed            as VU
import           GHC.Float
import           PetaVision.Utility.Parallel
import           Prelude                        as P
import           System.IO


fisherVectorSigma :: GMM
                  -> V.Vector Double
                  -> V.Vector GMMData
                  -> VU.Vector Double
fisherVectorSigma gmm@(MixtureModel n modelVec) zs xs =
  VU.fromList . P.concat . V.toList . V.map (toListDense) $ newSigmaK
  where !numData = P.fromIntegral . V.length $ xs
        !newSigmaK =
          V.map (\gmk@(Model (wk,(Gaussian _nd _muK sigmaK))) ->
                   scalarMulVec ((2 * numData * wk) ** (-0.5)) .
                   addFoldVec .
                   V.zipWith (\z x ->
                                let !assignment = assignPoint gmk z x
                                in scalarMulVec assignment .
                                   scalarAddVec (-1) . powVec 2 $
                                   (x / sigmaK))
                             zs $
                   xs)
                modelVec

fisherVectorConduit
  :: ParallelParams -> GMM -> Conduit (V.Vector GMMData) IO (VU.Vector Double)
fisherVectorConduit parallelParams gmm =
  do xs <- CL.take (batchSize parallelParams)
     if P.length xs > 0
        then let !ys =
                   parMapChunk
                     parallelParams
                     rdeepseq
                     (\x ->
                        let !z =
                              V.map (\y ->
                                       V.foldl' (\s (Model (wj,mj)) ->
                                                   s + (wj * gaussian mj y))
                                                0 $
                                       (model gmm))
                                    x
                            vec = fisherVectorSigma gmm z x
                            powerNormVec =
                              VU.map (\x ->
                                        if x > 0
                                           then x ** (0.5)
                                           else -((-x) ** (0.5)))
                                     vec
                            !l2Norm =
                              sqrt (VU.foldl' (\a b -> a + b ^ 2) 0 vec)
                        in VU.map (/ l2Norm) vec)
                     xs
             in do sourceList ys
                   fisherVectorConduit parallelParams gmm
        else return ()


fisherVectorTestSink
  :: ParallelParams -> GMM -> Sink (V.Vector GMMData) IO ()
fisherVectorTestSink parallelParams gmm =
  do x <- await
     case x of
       Nothing -> liftIO $ P.putStrLn "No data"
       Just y ->
         let !zs =
               V.map (\y' ->
                        V.foldl' (\s (Model (wj,mj)) ->
                                    s + (wj * gaussian mj y'))
                                 0 $
                        (model gmm))
                     y
             result = VU.toList $ fisherVectorSigma gmm zs y
             gm0 = (model gmm) V.! 0
             gm1 = (model gmm) V.! 1
             w1 = (\(Model (w1',_)) -> w1') gm1
             w0 = (\(Model (w0',_)) -> w0') gm0
             !numData = P.fromIntegral . V.length $ y
             hehe =
               V.zipWith (\z x' ->
                            (assignPoint gm1 z x') / (w1) -
                            (assignPoint gm0
                                         (V.head zs)
                                         x') /
                            (w0))
                         zs
                         y
             haha = (numData * (1 / w1 + 1 / w0)) ** (-0.5)
         in do liftIO $ print . P.take 5 $ result
               liftIO $ print $ V.take 5 $ hehe
               liftIO $ print haha
               liftIO $ print $ V.sum hehe
               liftIO $
                 print $
                  haha * (V.sum hehe)


