{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpticML.Parametric
  ( Para(..),
    liftPara,
    composePara
  )
where

import OpticML.Lenses (Lens, alongside, Lens')
import OpticML.LensImpl (p2, assocL, identityL, add, linear)
import Data.Matrix (Matrix, fromLists, zero)

data Para p s t a b = Para
    { params :: p
    , plens :: Lens s t a b
    }

type Para' p s a = Para p s s a a

liftPara :: Lens s t a b -> Para (Matrix p) (c, s) (c, t) a b
liftPara l = Para (fromLists [[]]) (p2 . l)

-- composePara :: forall p c2 o m n c q1 q2 . Para p (c2, o) (c2, o) (q1, q2) (q1, q2)
--                                         -> Para p (q1, q2) (q1, q2) (c, (q1, q2))      (c, (q1, q2)) 
--                                         -> Para p (c2, o) (c2, o) (c, (q1, q2))      (c, (q1, q2)) 
-- composePara :: Para p (b1, c) (b1, c) a b
--             -> Para p (a1, a) (a1, b) m n
--             -> Para p ((a1, b1), c) ((a1, b1), c) m n
-- composePara :: forall p c2 o t3 b3 c a b j k. 
--         Para p (c2, o) (c2, o) a b 
--     ->  Para p (c, a) (c, b) j k
--     ->  Para p ((c, c2), o) ((c, c2), o) j k
composePara :: forall p s1 s2 a b m n c q . Para p (s1, s2) (s1, s2) a b
                                        ->  Para q (c, a) (c, b) m n
                                        ->  Para (q, p) ((c, s1), s2) ((c, s1), s2) m n
composePara (Para p1 l1) (Para p2 l2) = Para cp cl
    where
        cp :: (q, p)
        cp = (p2, p1)

        x :: Lens (x, (s1, s2)) (x, (s1, s2)) (x, a) (x, b)
        x = identityL `alongside` l1

        tr :: Lens ((c, s1), s2) ((c, s1), s2) (c, a) (c, b)
        tr = assocL . x

        cl :: Lens ((c, s1), s2) ((c, s1), s2) m n
        cl = assocL . (identityL `alongside` l1) . l2

-- Lens (y, z) t a b
-- Lens (x, a) q r s

-- Lens x x x x

-- ((x, y), z)
-- (x, (y, z))
-- (x, a)
-- r

-- ((x, y), z) -> (x, (y, z)) -> (x, f (y, z)) -> g (x, f (y, z))

-- lp :: Num a => Para' p (c, (M a, M a)) (M a)
-- lp :: Num a => Para' p (s1, (M a, M a)) (M a)
-- lp :: Para' (Matrix p) (Matrix a, Matrix a) (Matrix a)

-- ap :: Num a => Para' (Matrix p) (c, (M a, M a)) (M a)

-- q :: Num a => Para p ((M a, M a), M a) ((M a, M a), M a) (M a) (M a)
-- q :: Para p ((c1, c2), (M a1, M a1)) ((c1, c2), (M a1, M a1)) (M a2) (M a2)
-- q :: Para p ((c1, c2), (M a1, M a1)) ((c1, c2), (M a1, M a1)) (M a2) (M a2)
-- q :: Num a => Para (Matrix p, Matrix q) (Mt a, M a) (Mt a, M a) (M a) (M a)
-- q = composePara lp ap

-- comp :: Num a => Lens' (Mt a, M a) (M a)
-- comp :: (Num p1, Num p2, Num a) => Para' (M p1, M p2) ((c, M a), M a) (M a)
-- comp = composePara lp ap
--     where
--         -- p :: Num p => M p
--         p = fromLists [[]]

--         -- lp :: Num p => Para' (M p) (M a, M a) (M a)
--         lp = Para p linear

--         ap :: Num a => Para' (M p) (c, (M a, M a)) (M a)
--         ap = liftPara add

-- MANUAL WORKING BELOW

-- ap :: Num a => Para' (M p) (c, (M a, M a)) (M a)
-- ap = liftPara add

-- type M a = Matrix a
-- type Mt a = (M a, M a)

-- y :: Num a => Lens' (c, Mt a) (c, M a)
-- y = identityL `alongside` linear

-- z :: Num a => Lens' ((c, M a), M a) (c, M a)
-- z = assocL . y

-- x :: Num a => Lens' (Mt a, M a) (M a)
-- x = z . add
