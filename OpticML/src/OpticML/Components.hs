{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module OpticML.Components
  ( dense,
    linearP,
    biasP
  )
where

import OpticML.Parametric (Para(..), Para', (|.|), liftPara, EmptyParam(..))
import System.Random (Random)
import Data.Random.Normal ( mkNormals )
import Numeric.LinearAlgebra (Matrix, Vector, (><), Konst (konst), Numeric)
import OpticML.LensImpl (linear, add)
import OpticML.Lenses (Lens')

seed :: Int
seed = 384723978470123987

dense :: (Numeric a, Floating a, Random a, Num (Vector a)) => (Int, Int) -> Lens' (Vector a) (Vector a) -> Para' (EmptyParam, (Vector a, Matrix a)) ((EmptyParam, (Vector a, Matrix a)), Vector a) (Vector a)
dense (a, b) act = linearP (a, b) |.| biasP b |.| liftPara act

linearP :: (Numeric a, Floating a, Random a) => (Int, Int) -> Para' (Matrix a) (Matrix a, Vector a) (Vector a)
linearP (a, b) = Para (b >< a $ take (a * b) (mkNormals seed)) linear

biasP :: (Numeric a, Num (Vector a)) => Int -> Para' (Vector a) (Vector a, Vector a) (Vector a)
biasP b = Para (konst 0 b) add