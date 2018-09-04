module Application.PVPAnalysis.Analysis where

import           Control.Arrow
import           Control.Monad                as M
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Maybe
import           Data.Vector.Unboxed          as VU
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel
import           Prelude                      as P
import           Text.Printf
import Data.Bits


sparsity :: PVPHeader -> ConduitT PVPOutputData () (ResourceT IO) ()
sparsity header =
  do numActive <-
       CL.fold (\b a ->
                  case a of
                    PVP_OUTPUT_ACT _ _ ->
                      error "Cannot compute PVP_OUTPUT_ACT file's sparsity"
                    PVP_OUTPUT_NONSPIKING_ACT _ x ->
                      b + (VU.length . VU.filter (/= 0) $ x)
                    PVP_OUTPUT_ACT_SPARSEVALUES _ x -> b + (VU.length x)
                    _ ->
                      error $
                      "computeActivationHistogram: " L.++ show a L.++
                      " is not supported.")
               0
     let percent = fromIntegral numActive / fromIntegral totalNumEle
     liftIO $
       printf "%0.1f%%(%d/%d Avg. Activatived)\n"
              (percent * 100 :: Double)
              (round $ percent * fromIntegral (nf header) :: Int)
              (nf header)
  where totalNumEle = (nBands header) * (nx header) * (ny header) * (nf header)

sparsityActivationHistogramSink ::
     PVPHeader
  -> ConduitT PVPOutputData () (ResourceT IO) (Double, VU.Vector Double)
sparsityActivationHistogramSink header =
  do (numActive,actCount) <-
       CL.fold (\(b,vec) a ->
                  let newB =
                        case a of
                          PVP_OUTPUT_ACT _ _ ->
                            error "Cannot compute PVP_OUTPUT_ACT file's sparsity"
                          PVP_OUTPUT_NONSPIKING_ACT _ x ->
                            b + (VU.length . VU.filter (/= 0) $ x)
                          PVP_OUTPUT_ACT_SPARSEVALUES _ x -> b + (VU.length x)
                          _ ->
                            error $
                            "computeActivationHistogram: " L.++ show a L.++
                            " is not supported."
                      newVec = computeActivationHistogram vec a
                  in (newB,newVec))
               (0
               ,VU.replicate ((nx header) * (ny header) * (nf header))
                             0)
     let totalNumEle =
           (nBands header) * (nx header) * (ny header) * (nf header)
         percent = fromIntegral numActive / fromIntegral totalNumEle
         actCountVec =
           toUnboxed .
           computeS .
           R.map (\x -> (fromIntegral x :: Double) / (fromIntegral ((nBands header) * (nx header) * (ny header)))) .
           sumS .
           sumS .
           R.backpermute (Z :. nf header :. ny header :. nx header)
                         (\(Z :. k :. j :. i) -> (Z :. j :. i :. k)) .
           fromUnboxed (Z :. ny header :. nx header :. nf header) $
           actCount
     liftIO $
       printf "Sparsity: %0.1f%%(%d/%d Avg. Activatived)\n"
              (percent * 100 :: Double)
              (round $ percent * fromIntegral (nf header) :: Int)
              (nf header)
     return (percent,actCountVec)

{-# INLINE computeActivationHistogram #-}

computeActivationHistogram
  :: VU.Vector Int -> PVPOutputData -> VU.Vector Int
computeActivationHistogram defaultVec (PVP_OUTPUT_NONSPIKING_ACT pvpDimension x) =
  VU.zipWith (+) defaultVec .
  VU.map (\y ->
            if y /= 0
               then 1
               else 0) $
  x
computeActivationHistogram defaultVec (PVP_OUTPUT_ACT_SPARSEVALUES _ x) =
  VU.accumulate (+) defaultVec . VU.map (second $ const 1) $ x
computeActivationHistogram _ x =
  error $ "computeActivationHistogram: " L.++ show x L.++ " is not supported."
  
{-# INLINE computeFeatureHistogram #-}

computeFeatureHistogram
  :: VU.Vector Int -> PVPOutputData -> VU.Vector Int
computeFeatureHistogram defaultVec (PVP_OUTPUT_NONSPIKING_ACT pvpDimension x) =
  VU.accumulate (+) defaultVec .
  VU.imap
    (\i y ->
       ( getFeatureIndex pvpDimension i
       , if y > 0
           then 1
           else 0)) $
  x
computeFeatureHistogram defaultVec (PVP_OUTPUT_ACT_SPARSEVALUES pvpDimension x) =
  VU.accumulate (+) defaultVec .
  VU.map (\(i, y) -> (getFeatureIndex pvpDimension i, 1)) $
  x
computeFeatureHistogram _ x =
  error $ "computeFeatureHistogram: " L.++ show x L.++ " is not supported."

{-# INLINE getFeatureIndex #-}

getFeatureIndex :: PVPDimension ->  Int -> Int
getFeatureIndex (PVPDimension nx' ny' nf') i =
  let n1 = nf' * nx'
      n2 = nf'
      n3 = (mod i n1)
      a = mod n3 n2
  in a


averageError :: PVPHeader -> ConduitT PVPOutputData () (ResourceT IO) ()
averageError header = do
  errorSum <-
    CL.fold
      (\b a ->
          case a of
            PVP_OUTPUT_NONSPIKING_ACT _ x ->
              b + sqrt (VU.sum . VU.map (^ (2 :: Int)) $ x)
            _ -> error "sparsity: pvpFile type is not supported.")
      0
  let numEle = nx header * ny header * nf header
      avgError = errorSum / fromIntegral numEle / fromIntegral (nBands header)
  liftIO . print $ avgError

weightChangeConduit :: ConduitT PVPOutputData Double (ResourceT IO) ()
weightChangeConduit = do
  x <- await
  when
    (isJust x)
    (do y <- peek
        when
          (isJust y)
          (do let a = extractW . fromJust $ x
                  b = extractW . fromJust $ y
                  (Z :. nf :. ny :. nx :. nc) = extent a
                  diff = R.sumAllS $ R.zipWith (\c d -> sqrt $ (c - d) ^ (2 :: Int)) a b
              yield $ diff / (fromIntegral $ nf * ny * nx * nc)
              weightChangeConduit))
  where
    extractW (PVP_OUTPUT_KERNEL arr) = arr
    extractW _ =
      error "weightChangeConduit: pvp file type is not PVP_OUTPUT_KERNEL."


diffXORPVPOutputData :: PVPOutputData -> PVPOutputData -> Int
diffXORPVPOutputData (PVP_OUTPUT_ACT_SPARSEVALUES _ xs) (PVP_OUTPUT_ACT_SPARSEVALUES _ ys) =
  diffXOR (VU.toList xs) (VU.toList ys)
diffXORPVPOutputData (PVP_OUTPUT_NONSPIKING_ACT _ xs) (PVP_OUTPUT_NONSPIKING_ACT _ ys) =
  VU.sum $
  VU.zipWith
    (\x y ->
       let a =
             if x > 0
               then 1
               else 0
           b =
             if y > 0
               then 1
               else 0
       in a `xor` b)
    xs
    ys

{-# INLINE diffXOR #-}

diffXOR :: [(Int, Double)] -> [(Int, Double)] -> Int
diffXOR xs [] = L.length xs
diffXOR [] ys = L.length ys
diffXOR (x@(i, _):xs) (y@(j, _):ys) 
  | i == j = diffXOR xs ys
  | i > j  = 1 + diffXOR (x:xs) ys
  | otherwise = 1 + diffXOR xs (y:ys)
  

diffACTPVPOutputData :: PVPOutputData -> PVPOutputData -> Double
diffACTPVPOutputData (PVP_OUTPUT_ACT_SPARSEVALUES _ xs) (PVP_OUTPUT_ACT_SPARSEVALUES _ ys) =
  (sqrt $ diffACT (VU.toList xs) (VU.toList ys)) / (sqrt . L.sum . L.map (^ (2 :: Int)) . L.map snd . VU.toList $ xs)
diffACTPVPOutputData (PVP_OUTPUT_NONSPIKING_ACT _ xs) (PVP_OUTPUT_NONSPIKING_ACT _ ys) =
  (sqrt . VU.sum $ VU.zipWith (\x y -> (x - y) ^ (2 :: Int)) xs ys) /
  (VU.sum . VU.map (^ (2 :: Int)) $ xs)


{-# INLINE diffACT #-}

diffACT :: [(Int, Double)] -> [(Int, Double)] -> Double
diffACT xs [] = L.sum . L.map ((^ (2 :: Int)) . snd) $ xs
diffACT [] ys = L.sum . L.map ((^ (2 :: Int)) . snd) $ ys
diffACT (x@(i, xv):xs) (y@(j, yv):ys)
  | i == j = ((xv - yv) ^ (2 :: Int)) + diffACT xs ys
  | i > j = (yv ^ (2 :: Int)) + diffACT (x : xs) ys
  | otherwise = ( xv ^ (2 :: Int)) + diffACT xs (y : ys)
