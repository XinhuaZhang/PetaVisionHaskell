module PetaVision.PVPFile.Utility where

import qualified Data.Array.Accelerate as A
import           Data.Array.Unboxed    as AU

sparse2Nonsparse
  :: (Int,Int,Int) -> [(Int,Double)] -> AU.Array (Int,Int,Int) Double
sparse2Nonsparse (nf,nx,ny) frame =
  accumArray (+)
             0
             ((0,0,0),(nf - 1,nx - 1,ny - 1)) $
  map (\(i,v) -> (indexMapping i,v)) frame
  where indexMapping :: Int -> (Int,Int,Int)
        indexMapping i = (a,b,c)
          where n1 = nf * nx
                n2 = nf
                c = div i n1
                n3 = (mod i n1)
                b = div n3 n2
                a = mod n3 n2
