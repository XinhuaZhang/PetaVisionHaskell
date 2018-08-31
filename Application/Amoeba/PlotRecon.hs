import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           Data.Maybe                   as Maybe
import           PetaVision.Data.Weight
import           PetaVision.Image.ImageIO
import           PetaVision.PVPFile.IO
import           Prelude                      as P
import           System.Directory
import           System.Environment

main = do
  (reconFilePath:idxStr:reconName:_) <- getArgs
  let idx = read idxStr :: Int
  recons <-
    runConduitRes $
    pvpFileSource reconFilePath .| (CL.drop idx >> CL.map id) .| CL.head
  case recons of
    Nothing -> error "idx is out of boundary."
    Just recon ->
      let arr = pvpOutputData2Array recon
          (Z :. _ :. _ :. nf) = extent arr
      in do createDirectoryIfMissing True "Recon"
            M.mapM_
              (\i ->
                 let x =
                       Image 8 .
                       computeUnboxedS .
                       extend (Z :. (1 :: Int) :. All :. All) . R.slice arr $
                       (Z :. All :. All :. i)
                 in plotImageRepa
                      ("Recon/" L.++ reconName L.++ "_" L.++ show i L.++ ".png")
                      x)
              [0 .. nf - 1]
