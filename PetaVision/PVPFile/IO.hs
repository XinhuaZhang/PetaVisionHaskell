module PetaVision.PVPFile.IO
       (readPVPHeader, sparseActivityPVPFileSource) where

import           Control.Monad.IO.Class
import           Data.Binary.Get
import qualified Data.ByteString.Lazy          as L
import           Data.Conduit
import           GHC.Float
import           System.IO

type PVPHeader = [Int]  

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
     return $
       ((fmap fromIntegral
              [headerSize
              ,numParams
              ,fileType
              ,nx
              ,ny
              ,nf
              ,numRecords
              ,recordSize
              ,dataSize
              ,dataType
              ,nxProcs
              ,nyProcs
              ,nxGlobal
              ,nyGlobal
              ,kx
              ,ky
              ,nb
              ,nBands]) ++
        [(round time)])

getPVPHeader :: Handle -> IO PVPHeader
getPVPHeader h =
  do bs <- L.hGet h 80
     let params = runGet getHeaderParam bs
     if (params !! 1 == 20)
        then return $ params
        else if ((params !! 1) > 20)
                then do bs <- L.hGet h (4 * ((params !! 1) - 20))
                        return $ params
                else return []
                
readPVPHeader :: FilePath -> IO PVPHeader
readPVPHeader filePath =
  do h <- openBinaryFile filePath ReadMode
     header <- getPVPHeader h
     hClose h
     return header

getPVPValueHeader :: Get (Double,Int)
getPVPValueHeader =
  do time <- getDoublele
     num <- getWord32le
     return $ (time,fromIntegral num)

{- PVP Filty Type 2 -}
getPVPValueInd :: Get Int
getPVPValueInd =
  do ind <- getWord32le
     return $ fromIntegral ind

getPVPValueType2 :: Get [Int]
getPVPValueType2 =
  do empty <- isEmpty
     if empty
        then return []
        else do p <- getPVPValueInd
                ps <- getPVPValueType2
                return (p : ps)


readSparseActivityFrameType2
  :: Handle -> IO [Int]
readSparseActivityFrameType2 h =
  do bs <- L.hGet h 12
     let (time,numActive) = runGet getPVPValueHeader bs
     bs' <- L.hGet h (4 * numActive)
     return $ runGet getPVPValueType2 bs'

sparseActivityPVPFileSourceType2
  :: FilePath -> IO (PVPHeader,Source IO [Int])
sparseActivityPVPFileSourceType2 filePath =
  do h <- openBinaryFile filePath ReadMode
     header <- getPVPHeader h
     let nbands = header !! 17
         fileType = header !! 2
     if fileType == 2
        then return (header,loop nbands h)
        else error "Wrong PVP file type!"
  where loop
          :: Int -> Handle -> Source IO [Int]
        loop n handle =
          do if n > 0
                then do frame <- liftIO $ readSparseActivityFrameType2 handle
                        yield frame
                        loop (n - 1) handle
                else do liftIO $ hClose handle
                        return ()

{- PVP Filty Type 6 -}
getPVPValuePair :: Get (Int,Double)
getPVPValuePair =
  do ind <- getWord32le
     v <- getFloatle
     return $ (fromIntegral ind,float2Double v)

getPVPValue :: Get [(Int,Double)]
getPVPValue =
  do empty <- isEmpty
     if empty
        then return []
        else do p <- getPVPValuePair
                ps <- getPVPValue
                return (p : ps)

readSparseActivityFrame
  :: Handle -> IO [(Int,Double)]
readSparseActivityFrame h =
  do bs <- L.hGet h 12
     let (time,numActive) = runGet getPVPValueHeader bs
     bs' <- L.hGet h (2 * 4 * (numActive))
     return $ runGet getPVPValue bs'

sparseActivityPVPFileSource
  :: FilePath -> IO (PVPHeader,Source IO [(Int,Double)])
sparseActivityPVPFileSource filePath =
  do h <- openBinaryFile filePath ReadMode
     header <- getPVPHeader h
     let nbands = header !! 17
         fileType = header !! 2
     if fileType == 6
        then return (header,loop nbands h)
        else error "Wrong PVP file type!"
  where loop
          :: Int -> Handle -> Source IO [(Int,Double)]
        loop n handle =
          do if n > 0
                then do frame <- liftIO $ readSparseActivityFrame handle
                        yield frame
                        loop (n - 1) handle
                else do liftIO $ hClose handle
                        return ()


