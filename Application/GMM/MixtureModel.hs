{-# LANGUAGE DeriveGeneric #-}
module Application.GMM.MixtureModel where

import           Application.GMM.Gaussian
import           Data.Binary
import           Data.Vector              as V
import           GHC.Generics
import           Prelude                  as P


newtype Model a =
  Model (Double,a)
  deriving (Show,Generic)

instance (Binary a) =>
         Binary (Model a) where
  put (Model x) = put x
  get =
    do x <- get
       return $ Model x

data MixtureModel a =
  MixtureModel {numModel :: Int
               ,model    :: V.Vector (Model a)}
  deriving (Show,Generic)

instance (Binary a) =>
         Binary (MixtureModel a) where
  put (MixtureModel numModel' xs) =
    do put numModel'
       put $ V.toList xs
  get =
    do numModel' <- get
       xs <- get
       return (MixtureModel numModel'
                            (V.fromList xs))


