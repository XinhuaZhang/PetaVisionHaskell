module Application.PVPAnalysis.Analysis where

import           Control.Monad.IO.Class (liftIO)
import           Data.Conduit
import           Data.Conduit.List      as CL
import           PetaVision.PVPFile.IO
import           Prelude                as P


sparsity :: PVPHeader -> Sink PVPOutputData IO ()
sparsity header =
  do numActive <-
       CL.fold (\b a ->
                  case a of
                    PVP_ACT _ -> error "Cannot compute PVP_ACT file's sparsity"
                    PVP_NONSPIKING_ACT _ ->
                      error "Cannot compute PVP_NONSPIKING_ACT file's sparsity"
                    PVP_ACT_SPARSEVALUES x -> b + P.length x)
               0
     liftIO $
       putStrLn $
       show (fromIntegral numActive / fromIntegral totalNumEle * 100) P.++ "%"
  where totalNumEle = (nBands header) * (nx header) * (ny header) * (nf header)
