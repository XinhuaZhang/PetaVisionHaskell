import           Control.Monad
import           Data.Array.Repa          as R
import           Data.List                as L
import           PetaVision.Image.ImageIO
import           System.Directory
import           System.Environment
import           System.FilePath

main = do
  args <- getArgs
  let (size:depth:_) = L.map (\x -> read x :: Int) . L.take 2 $ args
      path = L.last args
  when (depth /= 1 && depth /= 3) (error $ "Depth is wrong: " L.++ show depth)
  createDirectoryIfMissing True . takeDirectory . dropExtension $ path
  plotImageRepa path .
    Image 8 . computeS . fromFunction (Z :. depth :. size :. size) $
    (const 0)
