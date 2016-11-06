module Application.PVPAnalysis.Analysis where

import           Control.Monad.IO.Class (liftIO)
import           Data.Conduit
import           Data.Conduit.List      as CL
import           Data.List              as L
import           PetaVision.PVPFile.IO
import           Prelude                as P
import           Text.Printf


sparsity :: PVPHeader -> Sink PVPOutputData IO ()
sparsity header =
  do numActive <-
       CL.fold (\b a ->
                  case a of
                    PVP_OUTPUT_ACT _ _ ->
                      error "Cannot compute PVP_OUTPUT_ACT file's sparsity"
                    PVP_OUTPUT_NONSPIKING_ACT _ x ->
                      b + (P.length . P.filter (/= 0) $ x)
                    PVP_OUTPUT_ACT_SPARSEVALUES _ x -> b + (P.length x))
               0
     let percent = fromIntegral numActive / fromIntegral totalNumEle
     liftIO $
       printf "%0.1f%%(%d/%d Avg. Activatived)\n"
              (percent * 100 :: Double)
              (round $ percent * fromIntegral (nf header) :: Int)
              (nf header)
  where totalNumEle = (nBands header) * (nx header) * (ny header) * (nf header)
