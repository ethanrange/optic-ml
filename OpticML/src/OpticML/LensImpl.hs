module OpticML.LensImpl
  ( identityL,
    assocL, assocR,
    p1, p2,
    add,
    linear
  )
where

import OpticML.Lenses (Lens, lens)
import Control.Monad (join)
import Data.Matrix

-- Identity Lens

identityL :: Lens a a a a
identityL = lens id snd

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
add = lens v u
    where
        v :: Num i => (i, i) -> i
        v = uncurry (+)

        u :: Num i => (i, (i, i)) -> (i, i)
        u = join (,) . fst
        
linear :: Num a => Lens (Matrix a, Matrix a) (Matrix a, Matrix a) (Matrix a) (Matrix a)
linear = lens v u
    where
        v :: Num a => (Matrix a, Matrix a) -> Matrix a
        v = uncurry multStd

        u :: Num a => (Matrix a, (Matrix a, Matrix a)) -> (Matrix a, Matrix a)
        u (y, (m, x)) = (outer y x, multStd (transpose m) y)

        outer :: Num a => Matrix a -> Matrix a -> Matrix a
        outer v1 v2 = fromLists [[u * v | v <- ex v2] | u <- ex v1]
            where
                ex :: Matrix a -> [a]
                ex = head . toLists . transpose