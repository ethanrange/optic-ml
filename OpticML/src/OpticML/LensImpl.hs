{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpticML.LensImpl
  ( identityL,
    assocL, assocR,
    p1, p2,
    add,
    linear,
    lr,
    update,
    mse
  )
where

import OpticML.Lenses (Lens, lens, Lens')
import Control.Monad (join)
import Data.Matrix

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

assocL :: Lens ((a, b), c) ((a, b), c) (a, (b, c)) (a, (b, c))
assocL = lens assocLfwd (assocRfwd . fst)
{-# INLINE assocL #-}

assocR :: Lens (a, (b, c)) (a, (b, c)) ((a, b), c) ((a, b), c)
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

linear :: Num a => Lens (Matrix a, Matrix a) (Matrix a, Matrix a) (Matrix a) (Matrix a)
linear = lens v u
    where
        v :: Num a => (Matrix a, Matrix a) -> Matrix a
        v = uncurry (*)

        u :: Num a => (Matrix a, (Matrix a, Matrix a)) -> (Matrix a, Matrix a)
        u (y, (m, x)) = (outer y x, multStd (transpose m) y)

        outer :: Num a => Matrix a -> Matrix a -> Matrix a
        outer v1 v2 = fromLists [[e1 * e2 | e2 <- ex v2] | e1 <- ex v1]
            where
                ex :: Matrix a -> [a]
                ex = head . toLists . transpose

-- Learning Rate Lens

lr :: Double -> Lens s Double () ()
lr e = lens (const ()) (const e)

-- Update Lens

update :: Num a => Lens a a a a
update = lens id (uncurry (+))

-- MSE Lens

mse :: forall a . Fractional a => Lens' (Matrix a, Matrix a) a
mse = lens v u
    where
        hadamard :: Matrix a -> Matrix a -> Matrix a
        hadamard = elementwise (*)

        v :: (Matrix a, Matrix a) -> a
        v (y, ey)= 0.5 * (sum . toList) diffSq
            where
                diffSq = join hadamard (y - ey)

        u :: (a, (Matrix a, Matrix a)) -> (Matrix a, Matrix a)
        u (l, (y, ey)) = (ldp ey y, ldp y ey)
            where
                ldp :: Matrix a -> Matrix a -> Matrix a
                ldp a b = mapCol (const (* l)) 1 (a - b)
