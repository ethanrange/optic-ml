{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts #-}

module OpticML.LensImpl
  ( identityL,
    assocL, assocR,
    p1, p2,
    add,
    linear,
    lr,
    update,
    mse,
    relu, sigmoid
  )
where

import OpticML.Lenses (Lens, lens, Lens')
import Control.Monad (join)
import Numeric.LinearAlgebra (Matrix, Transposable (tr), Vector, (#>), outer, sumElements, Linear (scale), cmap, Numeric)
import Numeric.LinearAlgebra.Devel (zipVectorWith)

-- Identity Lens

identityL :: Lens a a a a
identityL = lens id fst

-- Associative Lenses

-- L Assoc to R Assoc
assocLfwd :: ((a, b), c) -> (a, (b, c))
assocLfwd ((x, y), z) = (x, (y, z))

-- R Assoc to L Assoc
assocRfwd :: (a, (b, c)) -> ((a, b), c)
assocRfwd (x, (y, z)) = ((x, y), z)

assocL :: Lens ((a, b), c) ((d, e), f) (a, (b, c)) (d, (e, f))
assocL = lens assocLfwd (assocRfwd . fst)
{-# INLINE assocL #-}

assocR :: Lens (a, (b, c)) (d, (e, f)) ((a, b), c) ((d, e), f)
assocR = lens assocRfwd (assocLfwd . fst)
{-# INLINE assocR #-}

-- Tuple Projection Lenses

p1 :: Lens (a, c) (b, c) a b
p1 = lens fst $ \(n, (_, b)) -> (n, b)
{-# INLINE p1 #-}

p2 :: Lens (x, a) (x, b) a b
p2 = lens snd $ \(n, (a, _)) -> (a, n)
{-# INLINE p2 #-}

-- Addition Lens

add :: Num a => Lens (a, a) (a, a) a a
add = lens v (join (,) . fst)
    where
        v :: Num i => (i, i) -> i
        v = uncurry (+)

linear :: forall a. Numeric a => Lens (Matrix a, Vector a) (Matrix a, Vector a) (Vector a) (Vector a)
linear = lens v u
    where
        v :: (Matrix a, Vector a) -> Vector a
        v = uncurry (#>)

        u :: (Vector a, (Matrix a, Vector a)) -> (Matrix a, Vector a)
        u (y, (m, x)) = (outer y x, tr m #> y)

-- Learning Rate Lens

lr :: Double -> Lens s Double () ()
lr e = lens (const ()) (const e)

-- Update Lens

update :: Num a => Lens a a a a
update = lens id (uncurry (+))

-- MSE Lens

mse :: forall a . (Fractional a, Numeric a, Num (Vector a)) => Lens' (Vector a, Vector a) a
mse = lens v u
    where
        v :: (Vector a, Vector a) -> a
        v (y, ey)= 0.5 * sumElements (join (*) (y - ey))

        u :: (a, (Vector a, Vector a)) -> (Vector a, Vector a)
        u (lr, (y, ey)) = (ey, scale lr (y - ey))

-- Activation lenses

relu :: forall a . (Numeric a, Ord a) => Lens' (Vector a) (Vector a)
relu = lens v u
    where
        v :: Vector a -> Vector a
        v = cmap (max 0)

        u :: (Vector a, Vector a) -> Vector a
        u (dy, x) = zipVectorWith (\yv xv -> if xv > 1 then yv else 0) dy x



sigmoid :: forall a . (Floating a, Numeric a) => Lens' (Vector a) (Vector a)
sigmoid = lens v u
    where
        v :: Vector a -> Vector a
        v = cmap sig

        u :: (Vector a, Vector a) -> Vector a
        u (dy, x) = zipVectorWith (\yv xv -> sig xv * (1 - sig xv) * yv) dy x

        sig :: a -> a
        sig x = 1 / (1 + exp (-x))
