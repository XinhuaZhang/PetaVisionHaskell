module Main where

import           Codec.Picture
import           Data.Conduit
import           Data.Conduit.List           as CL
import           PetaVision.Data.Weight
import           PetaVision.PVPFile.IO
import           Prelude                     as P
import           System.Directory
import           System.Environment

main =
  do (weightFile:folderName:_) <- getArgs
     weight <- pvpFileSource weightFile $$ CL.head
     dir <- getCurrentDirectory
     removePathForcibly (dir P.++ "/Weight/" P.++ folderName)
     createDirectoryIfMissing True
                              (dir P.++ "/Weight/" P.++ folderName)
     case weight of
       Nothing -> error "Read weight error"
       Just x ->
         do let w = (\(PVP_OUTPUT_KERNEL y) -> y) x
            plotWeight (dir P.++ "/Weight/" P.++ folderName)
                       w
