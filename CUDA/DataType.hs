module CUDA.DataType where

import           Data.Array.Accelerate as A

data GPUDataType = GPUFloat
                 | GPUDouble

data CUDAArrAcc = FloatArrAcc (Acc (Array DIM3 Float))
                | DoubleArrAcc (Acc (Array DIM3 Double))
