module PetaVision.PVPFile.IO
  (PVPHeader(..)
  ,PVPOutputData(..)
  ,PVPDimension(..)
  ,readPVPHeader
  ,pvpFileSource)
  where

import           Control.DeepSeq
import           Control.Monad                 as M
import           Control.Monad.IO.Class
import           Data.Array.Repa               as R
import           Data.Binary.Get
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BL
import           Data.Conduit                  as C
import           Data.List                     as L
import           Data.Vector.Unboxed           as VU
import           GHC.Float
import           Prelude                       as P
import           System.IO

data PVPHeader =
  PVPHeader {headerSize   :: Int
            ,numParams    :: Int
            ,fileType     :: Int
            ,nx           :: Int
            ,ny           :: Int
            ,nf           :: Int
            ,numRecords   :: Int
            ,recordSize   :: Int
            ,dataSize     :: Int
            ,dataType     :: Int
            ,nxProcs      :: Int
            ,nyProcs      :: Int
            ,nxGlobal     :: Int
            ,nyGlobal     :: Int
            ,kx           :: Int
            ,ky           :: Int
            ,nb           :: Int
            ,nBands       :: Int
            ,time         :: Double
            ,weightHeader :: PVPWeightHeader}

data PVPWeightHeader =
  PVPWeightHeader {nxp        :: Int
                  ,nyp        :: Int
                  ,nfp        :: Int
                  ,wMin       :: Double
                  ,wMax       :: Double
                  ,numPatches :: Int}
  deriving (Show)

data PVPFileType
  = PVP_FILE
  | PVP_ACT_FILE
  | PVP_WGT_FILE
  | PVP_NONSPIKING_ACT_FILE
  | PVP_KERNEL_FILE
  | PVP_ACT_SPARSEVALUES_FILE
  deriving (Show,Eq)

data PVPDataType
  = PV_BYTE
  | PV_INT
  | PV_FLOAT
  | PV_SPARSEVALUES
  deriving (Show)

data PVPDimension =
  PVPDimension {outputNx :: !Int
               ,outputNy :: !Int
               ,outputNf :: !Int}
  deriving (Show)

instance NFData PVPDimension where
  rnf (PVPDimension a b c) = a `seq` b `seq` c `seq` ()

data PVPOutputData
  = PVP_OUTPUT_ACT !PVPDimension ![Int]
  | PVP_OUTPUT_NONSPIKING_ACT !PVPDimension !(VU.Vector Double)
  | PVP_OUTPUT_ACT_SPARSEVALUES !PVPDimension !(VU.Vector (Int,Double))
  | PVP_OUTPUT_KERNEL !(Array U DIM4 Double)
  deriving (Show)

instance NFData PVPOutputData where
  rnf x =
    case x of
      PVP_OUTPUT_ACT d xs              -> d `seq` xs `seq` ()
      PVP_OUTPUT_NONSPIKING_ACT d xs   -> d `seq` xs `seq` ()
      PVP_OUTPUT_ACT_SPARSEVALUES d xs -> d `seq` xs `seq` ()



data PVPFrameData
  = FRAME_ACT !Int
  | FRAME_NONSPIKING_ACT !Double
  | FRAME_ACT_SPARSEVALUES !(Int,Double)
  | FRAME_KERNEL ![Double]
  deriving (Show)

getPVPFileType :: PVPHeader -> PVPFileType
getPVPFileType header =
  case (fileType header) of
    1 -> PVP_FILE
    2 -> PVP_ACT_FILE
    3 -> PVP_WGT_FILE
    4 -> PVP_NONSPIKING_ACT_FILE
    5 -> PVP_KERNEL_FILE
    6 -> PVP_ACT_SPARSEVALUES_FILE
    _ -> error "Wrong PVPFileType."


getPVPDataType :: PVPHeader -> PVPDataType
getPVPDataType header =
  case (dataType header) of
    1 -> PV_BYTE
    2 -> PV_INT
    3 -> PV_FLOAT
    4 -> PV_SPARSEVALUES
    _ -> error "Wrong PVPDataType."


-- Header functions
getHeaderParam :: Get PVPHeader
getHeaderParam =
  do headerSize' <- getWord32le
     numParams' <- getWord32le
     fileType' <- getWord32le
     nx' <- getWord32le
     ny' <- getWord32le
     nf' <- getWord32le
     numRecords' <- getWord32le
     recordSize' <- getWord32le
     dataSize' <- getWord32le
     dataType' <- getWord32le
     nxProcs' <- getWord32le
     nyProcs' <- getWord32le
     nxGlobal' <- getWord32le
     nyGlobal' <- getWord32le
     kx' <- getWord32le
     ky' <- getWord32le
     nb' <- getWord32le
     nBands' <- getWord32le
     time' <- getDoublele
     wHeader' <-
       if fileType' == 3 || fileType' == 5
          then getWeightHeaderParams
          else return $ PVPWeightHeader 0 0 0 0 0 0
     return $
       PVPHeader (fromIntegral headerSize')
                 (fromIntegral numParams')
                 (fromIntegral fileType')
                 (fromIntegral nx')
                 (fromIntegral ny')
                 (fromIntegral nf')
                 (fromIntegral numRecords')
                 (fromIntegral recordSize')
                 (fromIntegral dataSize')
                 (fromIntegral dataType')
                 (fromIntegral nxProcs')
                 (fromIntegral nyProcs')
                 (fromIntegral nxGlobal')
                 (fromIntegral nyGlobal')
                 (fromIntegral kx')
                 (fromIntegral ky')
                 (fromIntegral nb')
                 (fromIntegral nBands')
                 time'
                 wHeader'

getWeightHeaderParams :: Get PVPWeightHeader
getWeightHeaderParams =
  do nxp' <- getInt32le
     nyp' <- getInt32le
     nfp' <- getInt32le
     wMin' <- getFloatle
     wMax' <- getFloatle
     numPatches' <- getInt32le
     return $!
       PVPWeightHeader (fromIntegral nxp')
                       (fromIntegral nyp')
                       (fromIntegral nfp')
                       (float2Double wMin')
                       (float2Double wMax')
                       (fromIntegral numPatches')

getPVPHeader :: Handle -> IO PVPHeader
getPVPHeader h =
  do bs <- BL.hGet h 4
     let headerSize' = fromIntegral $ runGet getInt32le bs
     bs' <- BL.hGet h (headerSize' - 4)
     return $ runGet getHeaderParam (BL.append bs bs')

readPVPHeader :: FilePath -> IO PVPHeader
readPVPHeader filePath =
  do h <- openBinaryFile filePath ReadMode
     header <- getPVPHeader h
     hClose h
     return header

getPVPDimension :: PVPHeader -> PVPDimension
getPVPDimension header =
  PVPDimension (nx header)
               (ny header)
               (nf header)

-- Frame functions
getByteStringData
  :: Handle -> Int -> PVPDataType -> IO BL.ByteString
getByteStringData handle n dataType =
  case dataType of
    PV_BYTE         -> BL.hGet handle n
    PV_INT          -> BL.hGet handle (4 * n)
    PV_FLOAT        -> BL.hGet handle (4 * n)
    PV_SPARSEVALUES -> BL.hGet handle (4 * n)

getPVPFrameData :: PVPHeader -> Get PVPFrameData
getPVPFrameData header =
  case (getPVPFileType header) of
    PVP_FILE -> undefined
    PVP_ACT_FILE ->
      do ind <- getWord32le
         return $! FRAME_ACT (fromIntegral ind)
    PVP_WGT_FILE -> undefined
    PVP_NONSPIKING_ACT_FILE ->
      do val <- getFloatle
         return $! FRAME_NONSPIKING_ACT (float2Double val)
    PVP_KERNEL_FILE -> do _ <- getInt16le
                          _ <- getInt16le
                          _ <- getInt32le
                          xs <- M.replicateM (nxp' * nyp' * nfp') getFloatle
                          return $! FRAME_KERNEL . P.map float2Double $ xs
    PVP_ACT_SPARSEVALUES_FILE ->
      do ind <- getWord32le
         v <- getFloatle
         return $! FRAME_ACT_SPARSEVALUES (fromIntegral ind,float2Double v)
  where (PVPWeightHeader nxp' nyp' nfp' _ _ numPatches') = weightHeader header

incrementalGetPVPFrameData :: PVPHeader -> BL.ByteString -> [PVPFrameData]
incrementalGetPVPFrameData header input0 = go decoder input0
  where decoder = runGetIncremental (getPVPFrameData header)
        go
          :: Decoder PVPFrameData -> BL.ByteString -> [PVPFrameData]
        go (Done leftover' _consumed x) input
          | BS.null leftover' && BL.null input = [x]
          | otherwise = x : go decoder (BL.chunk leftover' input)
        go (Partial k) input =
          go (k . takeHeadChunk $ input)
             (dropHeadChunk input)
        go (Fail _leftover _consumed msg) _input = error msg

takeHeadChunk :: BL.ByteString -> Maybe BS.ByteString
takeHeadChunk lbs =
  case lbs of
    (BL.Chunk bs _) -> Just bs
    _               -> Nothing

dropHeadChunk :: BL.ByteString -> BL.ByteString
dropHeadChunk lbs =
  case lbs of
    (BL.Chunk _ lbs') -> lbs'
    _                 -> BL.Empty

getFrame
  :: PVPHeader -> Handle -> IO PVPOutputData
getFrame header h =
  case (getPVPFileType header) of
    PVP_FILE -> undefined
    PVP_ACT_FILE ->
      do bs <- BL.hGet h 12
         let (_time,numActive) =
               runGet (do time <- getDoublele
                          num <- getWord32le
                          return $ (time,fromIntegral num))
                      bs
         bs' <-
           getByteStringData h
                             numActive
                             (getPVPDataType header)
         return .
           PVP_OUTPUT_ACT (getPVPDimension header) .
           P.map (\(FRAME_ACT x) -> x) $
           incrementalGetPVPFrameData header bs'
    PVP_WGT_FILE -> undefined
    PVP_NONSPIKING_ACT_FILE ->
      do bs <- BL.hGet h 8
         let _time =
               runGet (do time <- getDoublele
                          return $ time)
                      bs
         bs' <-
           getByteStringData h
                             (recordSize header)
                             (getPVPDataType header)
         return .
           PVP_OUTPUT_NONSPIKING_ACT (getPVPDimension header) .
           VU.fromList . P.map (\(FRAME_NONSPIKING_ACT x) -> x) $
           incrementalGetPVPFrameData header bs'
    PVP_KERNEL_FILE ->
      do _frameHeader <- getPVPHeader h
         bs' <- BL.hGet h (recordSize header)
         let xs =
               P.concat . L.transpose . P.map (\(FRAME_KERNEL x) -> x) $
               incrementalGetPVPFrameData header bs'
         return .
           PVP_OUTPUT_KERNEL .
           fromListUnboxed (Z :. nyp' :. nxp' :. nfp' :. numPatches') $
           xs
    PVP_ACT_SPARSEVALUES_FILE ->
      do bs <- BL.hGet h 12
         let (_time,numActive) =
               runGet (do time <- getDoublele
                          num <- getWord32le
                          return $ (time,fromIntegral num))
                      bs
         if numActive == 0
            then return (PVP_OUTPUT_ACT_SPARSEVALUES (getPVPDimension header)
                                                     VU.empty)
            else do bs' <-
                      getByteStringData h
                                        (numActive * 2)
                                        (getPVPDataType header)
                    return .
                      PVP_OUTPUT_ACT_SPARSEVALUES (getPVPDimension header) .
                      VU.fromList . P.map (\(FRAME_ACT_SPARSEVALUES x) -> x) $
                      incrementalGetPVPFrameData header bs'
  where (PVPWeightHeader nxp' nyp' nfp' _ _ numPatches') = weightHeader header


pvpFileSource
  :: FilePath -> C.Source IO PVPOutputData
pvpFileSource filePath =
  do h <- liftIO $ openBinaryFile filePath ReadMode
     header <- liftIO $ getPVPHeader h
     let fileType' = getPVPFileType header
     h1 <-
       if fileType' == PVP_WGT_FILE || fileType' == PVP_KERNEL_FILE
          then do liftIO $ hClose h
                  liftIO $ openBinaryFile filePath ReadMode
          else return h
     loop (nBands header) h1 header
  where loop n handle header' =
          do if n > 0
                then do frame <- liftIO $ getFrame header' handle
                        yield frame
                        loop (n - 1) handle header'
                else do liftIO $ hClose handle
                        return ()
