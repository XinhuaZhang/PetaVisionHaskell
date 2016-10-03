module Application.PVP2LibLinear.Utility where

dimOffset
  :: [[Int]] -> [((Int,Int,Int),Int)]
dimOffset xs = zip ind offset
  where ind = map (\x -> (x !! 5,x !! 3,x !! 4)) xs
        offset =
          scanl (\x (nf,nx,ny) -> x + nf * nx * ny)
                0
                (init ind)
