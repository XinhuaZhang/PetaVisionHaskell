{-# LANGUAGE BangPatterns #-}
module Application.GMM.ConvertPVPGMMConduit where


import           Control.Monad                as M
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Binary
import           Data.ByteString              as BS
import           Data.ByteString.Lazy         as BL
import           Data.Conduit                 as C
import           Data.Conduit.Binary          as CB
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Vector.Unboxed          as VU
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel
import           System.IO
import GHC.Float

-- convert a PVP file to a list of numZero nonzeroEle1 nonzeroEle2 nonzeroEle3 ...
-- This is for GMM only. It is feature wise conversion


--input layout is ny x nx x nf
unpooledSparse2NonsparseConduit :: ParallelParams
                                -> Int
                                -> Conduit PVPOutputData IO (VU.Vector Double)
unpooledSparse2NonsparseConduit parallelParams ind = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\x ->
                    case x of
                      PVP_OUTPUT_ACT_SPARSEVALUES (PVPDimension nx' ny' nf') indX ->
                        let !vec =
                              VU.accum (+) (VU.replicate (nx' * ny' * nf') 0) indX
                            !arr = R.fromUnboxed (Z :. ny' :. nx' :. nf') vec
                        in VU.filter (/= 0) . toUnboxed . computeS $
                           R.slice arr (Z :. All :. All :. ind)
                      _ ->
                        error
                          "unpooledSparse2Nonsparse: PVP_OUTPUT_ACT_SPARSEVALUES is only one supported format")
                xs
        sourceList ys
        unpooledSparse2NonsparseConduit parallelParams ind)
        

unpooledSparse2NonsparseImageConduit :: ParallelParams
                                     -> Conduit PVPOutputData IO [(Int,VU.Vector Double)]
unpooledSparse2NonsparseImageConduit parallelParams = do
  xs <- CL.take (batchSize parallelParams)
  unless
    (L.null xs)
    (do let ys =
              parMapChunk
                parallelParams
                rdeepseq
                (\x ->
                    case x of
                      PVP_OUTPUT_ACT_SPARSEVALUES (PVPDimension nx' ny' nf') indX ->
                        let !vec =
                              VU.accum (+) (VU.replicate totalNumEle 0) indX
                            !arr = R.fromUnboxed (Z :. ny' :. nx' :. nf') vec
                            !totalNumEle = nx' * ny' * nf'
                        in L.map
                             (\ind ->
                                 let !vec1 =
                                       VU.filter (/= 0) . toUnboxed . computeS $
                                       R.slice arr (Z :. All :. All :. ind)
                                 in (totalNumEle - VU.length vec1, vec1) )
                             [0 .. nf' - 1]
                      _ ->
                        error
                          "unpooledSparse2Nonsparse: PVP_OUTPUT_ACT_SPARSEVALUES is only one supported format")
                xs
        sourceList ys
        unpooledSparse2NonsparseImageConduit parallelParams)

hFeatureSink :: Handle -> Int -> Int -> Sink (VU.Vector Double) IO Handle
hFeatureSink h totalNumImage imageSize = do
  liftIO $ BL.hPut h (encode (fromIntegral totalNumImage :: Word32)) -- 4 bytes
  liftIO $ BL.hPut h (encode (fromIntegral imageSize :: Word32)) -- 4 bytes
  CL.foldM
    (\handle x -> do
       let !bs = encode . L.map double2Float . VU.toList $ x
       BL.hPut handle (encode (fromIntegral . BL.length $ bs :: Word32)) -- 4 bytes
       BL.hPut handle bs
       return handle)
    h



featureConduit :: Conduit BS.ByteString (ResourceT IO) (Int,VU.Vector Double)
featureConduit = do
  totalImageNumBs <- CB.take 4
  unless
    (BL.length totalImageNumBs == 0)
    (do imageSizeBs <- CB.take 4
        let totalImageNum = fromIntegral (decode totalImageNumBs :: Word32) :: Int
            imageSize = fromIntegral (decode imageSizeBs :: Word32) :: Int
        xss <-
          M.replicateM
            totalImageNum
            (do sizeBs <- CB.take 4
                let size' = fromIntegral (decode sizeBs :: Word32) :: Int
                xsBs <- CB.take size'
                return . L.map float2Double . decode $ xsBs)
        let !vec = VU.fromList . L.concat $ xss
        yield (totalImageNum * imageSize - VU.length vec, vec)
        featureConduit)
