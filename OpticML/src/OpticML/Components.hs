{-# LANGUAGE RankNTypes #-}

module OpticML.Components
  ( dense,
    linearP,
    biasP
  )
where

import OpticML.Parametric (Para(..), Para', (|.|), liftPara, EmptyParam(..))
import System.Random (Random)
import Data.Random.Normal ( mkNormals )
import Data.Matrix (Matrix, zero, fromList)
import OpticML.LensImpl (linear, add)
import OpticML.Lenses (Lens')

seed :: Int
seed = 384723978470123987

dense :: (Floating a, Random a) => (Int, Int) -> Lens' (Matrix a) (Matrix a) -> Para' (EmptyParam, (Matrix a, Matrix a)) ((EmptyParam, (Matrix a, Matrix a)), Matrix a) (Matrix a)
dense (a, b) act = linearP (a, b) |.| biasP b |.| liftPara act

linearP :: (Floating a, Random a) => (Int, Int) -> Para' (Matrix a) (Matrix a, Matrix a) (Matrix a)
linearP (a, b) = Para (fromList b a $ take (a * b) (mkNormals seed)) linear

biasP :: Num a => Int -> Para' (Matrix a) (Matrix a, Matrix a) (Matrix a)
biasP b = Para (zero b 1) add