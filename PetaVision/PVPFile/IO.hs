module PetaVision.PVPFile.IO
  (PVPHeader(..)
  ,PVPOutputData(..)
  ,readPVPHeader
  ,pvpFileSource)
  where

import           Control.DeepSeq
import           Control.Monad.IO.Class
import           Data.Binary.Get
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as L
import           Data.Conduit
import           GHC.Float
import           System.IO

data PVPHeader =
  PVPHeader {headerSize :: Int
            ,numParams  :: Int
            ,fileType   :: Int
            ,nx         :: Int
            ,ny         :: Int
            ,nf         :: Int
            ,numRecords :: Int
            ,recordSize :: Int
            ,dataSize   :: Int
            ,dataType   :: Int
            ,nxProcs    :: Int
            ,nyProcs    :: Int
            ,nxGlobal   :: Int
            ,nyGlobal   :: Int
            ,kx         :: Int
            ,ky         :: Int
            ,nb         :: Int
            ,nBands     :: Int
            ,time       :: Double}

data PVPFileType
  = PVP_FILE
  | PVP_ACT_FILE
  | PVP_WGT_FILE
  | PVP_NONSPIKING_ACT_FILE
  | PVP_KERNEL_FILE
  | PVP_ACT_SPARSEVALUES_FILE
  deriving (Show)

data PVPDataType
  = PV_BYTE
  | PV_INT
  | PV_FLOAT
  | PV_SPARSEVALUES
  deriving (Show)

data PVPOutputData
  = PVP_ACT [Int]
  | PVP_NONSPIKING_ACT [Double]
  | PVP_ACT_SPARSEVALUES [(Int,Double)]
  deriving (Show)

instance NFData PVPOutputData where
  rnf x =
    case x of
      PVP_ACT xs              -> rnf xs
      PVP_NONSPIKING_ACT xs   -> rnf xs
      PVP_ACT_SPARSEVALUES xs -> rnf xs


data PVPFrameData
  = ACT !Int
  | NONSPIKING_ACT !Double
  | ACT_SPARSEVALUES !(Int,Double)
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
  do headerSize <- getWord32le
     numParams <- getWord32le
     fileType <- getWord32le
     nx <- getWord32le
     ny <- getWord32le
     nf <- getWord32le
     numRecords <- getWord32le
     recordSize <- getWord32le
     dataSize <- getWord32le
     dataType <- getWord32le
     nxProcs <- getWord32le
     nyProcs <- getWord32le
     nxGlobal <- getWord32le
     nyGlobal <- getWord32le
     kx <- getWord32le
     ky <- getWord32le
     nb <- getWord32le
     nBands <- getWord32le
     time <- getDoublele
     return $ PVPHeader (fromIntegral headerSize) 
                        (fromIntegral numParams)
                        (fromIntegral fileType)
                        (fromIntegral nx)
                        (fromIntegral ny)
                        (fromIntegral nf)
                        (fromIntegral numRecords )
                        (fromIntegral recordSize)
                        (fromIntegral dataSize)
                        (fromIntegral dataType)
                        (fromIntegral nxProcs)
                        (fromIntegral nyProcs)
                        (fromIntegral nxGlobal)
                        (fromIntegral nyGlobal)
                        (fromIntegral kx)
                        (fromIntegral ky)
                        (fromIntegral nb)
                        (fromIntegral nBands)
                        (time)


getPVPHeader :: Handle -> IO PVPHeader
getPVPHeader h =
  do bs <- L.hGet h 80
     let params = runGet getHeaderParam bs
     if ((numParams params) == 20)
        then return $ params
        else if ((numParams params) > 20)
                then do bs <- L.hGet h (4 * ((numParams params) - 20))
                        return $ params
                else error "there are not 20 pvp header parameters."

readPVPHeader :: FilePath -> IO PVPHeader
readPVPHeader filePath =
  do h <- openBinaryFile filePath ReadMode
     header <- getPVPHeader h
     hClose h
     return header

-- Frame functions
getByteStringData
  :: Handle -> Int -> PVPDataType -> IO L.ByteString
getByteStringData handle n dataType =
  case dataType of
    PV_BYTE         -> L.hGet handle n
    PV_INT          -> L.hGet handle (4 * n)
    PV_FLOAT        -> L.hGet handle (4 * n)
    PV_SPARSEVALUES -> L.hGet handle (2 * 4 * n)

getPVPFrameData :: PVPFileType -> Get PVPFrameData
getPVPFrameData fileType =
  case fileType of
    PVP_FILE -> undefined
    PVP_ACT_FILE ->
      do ind <- getWord32le
         return $! ACT (fromIntegral ind)
    PVP_WGT_FILE -> undefined
    PVP_NONSPIKING_ACT_FILE ->
      do val <- getFloatle
         return $! NONSPIKING_ACT (float2Double val)
    PVP_KERNEL_FILE -> undefined
    PVP_ACT_SPARSEVALUES_FILE ->
      do ind <- getWord32le
         v <- getFloatle
         return $! ACT_SPARSEVALUES (fromIntegral ind,float2Double v)

incrementalGetPVPFrameData :: PVPFileType -> L.ByteString -> PVPOutputData
incrementalGetPVPFrameData fileType input0 = go decoder input0
  where decoder = runGetIncremental (getPVPFrameData fileType)
        go
          :: Decoder PVPFrameData -> L.ByteString -> PVPOutputData
        go (Done leftover' _consumed (ACT x)) input
          | BS.null leftover' && L.null input = PVP_ACT [x]
          | otherwise =
            PVP_ACT (x :
                     ((\(PVP_ACT xs) -> xs) $!
                      go decoder (L.chunk leftover' input)))
        go (Done leftover' _consumed (NONSPIKING_ACT x)) input
          | BS.null leftover' && L.null input = PVP_NONSPIKING_ACT [x]
          | otherwise =
            PVP_NONSPIKING_ACT
              (x :
               ((\(PVP_NONSPIKING_ACT xs) -> xs) $!
                go decoder (L.chunk leftover' input)))
        go (Done leftover' _consumed (ACT_SPARSEVALUES x)) input
          | BS.null leftover' && L.null input = PVP_ACT_SPARSEVALUES [x]
          | otherwise =
            PVP_ACT_SPARSEVALUES
              (x :
               ((\(PVP_ACT_SPARSEVALUES xs) -> xs) $!
                go decoder (L.chunk leftover' input)))
        go (Partial k) input =
          go (k . takeHeadChunk $ input)
             (dropHeadChunk input)
        go (Fail _leftover _consumed msg) _input = error msg

takeHeadChunk :: L.ByteString -> Maybe BS.ByteString
takeHeadChunk lbs =
  case lbs of
    (L.Chunk bs _) -> Just bs
    _              -> Nothing

dropHeadChunk :: L.ByteString -> L.ByteString
dropHeadChunk lbs =
  case lbs of
    (L.Chunk _ lbs') -> lbs'
    _                -> L.Empty

getFrame
  :: Handle -> PVPFileType -> PVPDataType -> Int -> IO PVPOutputData
getFrame h fileType@PVP_ACT_SPARSEVALUES_FILE dataType _recordSize =
  do bs <- L.hGet h 12
     let (_time,numActive) =
           runGet (do time <- getDoublele
                      num <- getWord32le
                      return $ (time,fromIntegral num))
                  bs
     if numActive == 0
        then return (PVP_ACT_SPARSEVALUES [])
        else do bs' <- getByteStringData h numActive dataType
                return $ incrementalGetPVPFrameData fileType bs'
getFrame h fileType@PVP_ACT_FILE dataType _recordSize =
  do bs <- L.hGet h 12
     let (_time,numActive) =
           runGet (do time <- getDoublele
                      num <- getWord32le
                      return $ (time,fromIntegral num))
                  bs
     bs' <- getByteStringData h numActive dataType
     return $ incrementalGetPVPFrameData fileType bs'
getFrame h fileType@PVP_NONSPIKING_ACT_FILE dataType recordSize =
  do bs <- L.hGet h 8
     let _time =
           runGet (do time <- getDoublele
                      return $ time)
                  bs
     bs' <- getByteStringData h recordSize dataType
     return $ incrementalGetPVPFrameData fileType bs'

pvpFileSource
  :: FilePath -> Source IO PVPOutputData
pvpFileSource filePath =
  do h <- liftIO $ openBinaryFile filePath ReadMode
     header <- liftIO $ getPVPHeader h
     let fileType' = getPVPFileType header
         dataType' = getPVPDataType header
         loop n handle =
           do if n > 0
                 then do frame <- liftIO $ getFrame handle fileType' dataType' (recordSize header)
                         yield frame
                         loop (n - 1) handle
                 else do liftIO $ hClose handle
                         return ()
     loop (nBands header) h


