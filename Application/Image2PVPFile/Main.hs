import           Application.Image2PVPFile.Conduit
import           Control.Monad                     as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa                   as R
import           Data.Conduit
import           Data.Conduit.Binary               as CB
import           Data.Conduit.List                 as CL
import           Data.List                         as L
import           PetaVision.Data.Image.ImageIO
import           PetaVision.Data.Weight
import           PetaVision.PVPFile.IO
import           PetaVision.Utility.Parallel
import           System.Environment
import           System.IO

main =
  do (imageListPath:outputPath:_) <- getArgs
     numImage <- M.liftM L.length $ readImagePathList imageListPath
     let nx' = 128
         ny' = 128
         nf' = 3
         parallelParams = ParallelParams 8 100
         header =
           PVPHeader {headerSize = 80
                     ,numParams = 20
                     ,fileType = 4
                     ,nx = nx'
                     ,ny = ny'
                     ,nf = nf'
                     ,numRecords = 1
                     ,recordSize = nx' * ny' * nf'
                     ,dataSize = 4
                     ,dataType = 3
                     ,nxProcs = 1
                     ,nyProcs = 1
                     ,nxGlobal = nx'
                     ,nyGlobal = ny'
                     ,kx = 0
                     ,ky = 0
                     ,nb = 0
                     ,nBands = numImage
                     ,time = 0
                     ,weightHeader = undefined}
     withBinaryFile outputPath WriteMode $
       \h ->
         do hPutPVPHeader h header
            runResourceT $
              imagePathSource imageListPath $$ readImageConduit True =$=
              rescaleSquareImageConduit parallelParams
                                        (ny',nx')
                                        (0,255) =$=
              writePVPFileConduit =$=
              sinkHandle h
