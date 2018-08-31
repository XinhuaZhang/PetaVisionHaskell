import           Control.Monad                as M
import           Control.Monad.Trans.Resource
import           Data.Array.Repa              as R
import           Data.Conduit                 as C
import           Data.Conduit.List            as CL
import           Data.List                    as L
import           PetaVision.Image.ImageIO
import           PetaVision.PVPFile.IO
import           System.Directory
import           System.Environment
import           System.FilePath

main = do
  (filePath:_) <- getArgs
  header <- readPVPHeader filePath
  xs <- runResourceT $ pvpFileSource filePath $$ CL.take 5
  let ys =
        L.map
          (\x ->
             let y = pvpOutputData2Array x
             in L.map
                  (\i ->
                     Image 8 .
                     computeUnboxedS .
                     R.extend (Z :. (1 :: Int) :. All :. All) . R.slice y $
                     (Z :. All :. All :. i))
                  [0 .. nf header - 1])
          xs
  createDirectoryIfMissing True "Image"
  M.zipWithM_
    (\i y ->
       M.zipWithM_
         (\j z ->
            plotImageRepa
              ("Image/" L.++ show i L.++ "_" L.++ show j L.++ ".png")
              z)
         [1 ..]
         y)
    [1 ..]
    ys
