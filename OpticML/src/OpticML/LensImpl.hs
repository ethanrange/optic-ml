module OpticML.LensImpl
  ( assocL, assocR,
    p1, p2,
  )
where

import OpticML.Lenses (Lens, lens)

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
