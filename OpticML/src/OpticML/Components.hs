module OpticML.Components
  ( dense,
    linearP,
    biasP
  )
where

import OpticML.Parametric (Para(..), Para', (|.|))
import Data.Random.Normal ( mkNormals )
import Data.Matrix (Matrix, zero, fromList)
import OpticML.LensImpl (linear, add)

seed :: Int
seed = 384723978470123987

dense :: Num a => (Int, Int) -> Para' (Matrix Double, Matrix Double) ((Matrix a, Matrix a), Matrix a) (Matrix a)
dense (a, b) = linearP (a, b) |.| biasP b

linearP :: Num a => (Int, Int) -> Para' (Matrix Double) (Matrix a, Matrix a) (Matrix a)
linearP (a, b) = Para (fromList b a $ take (a * b) (mkNormals seed)) linear

biasP :: Num a => Int -> Para' (Matrix Double) (Matrix a, Matrix a) (Matrix a)
biasP b = Para (zero b 1) add