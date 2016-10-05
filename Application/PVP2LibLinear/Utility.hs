module Application.PVP2LibLinear.Utility where

import           PetaVision.PVPFile.IO (PVPHeader (..))

dimOffset :: [PVPHeader] -> [((Int,Int,Int),Int)]
dimOffset xs = zip ind offset
  where ind = map (\x -> (nf x,nx x,ny x)) xs
        offset =
          scanl (\x (nf',nx',ny') -> x + nf' * nx' * ny')
                0
                (init ind)
