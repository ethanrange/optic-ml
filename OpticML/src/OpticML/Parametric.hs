{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OpticML.Parametric
  ( Para(..), Para',
    liftPara,
    composePara, (|.|)
  )
where

import OpticML.Lenses (Lens, alongside)
import OpticML.LensImpl (p2, assocL, identityL)
import Data.Matrix (Matrix, fromLists)

data Para p s t a b = Para
    { params :: p
    , plens :: Lens s t a b
    }

type Para' p s a = Para p s s a a

instance (Num a, Num b) => Num (a,b) where
  fromInteger n   = (fromInteger n, fromInteger n)
  (a,b) + (a',b') = (a + a', b + b')
  (a,b) - (a',b') = (a - a', b - b')
  (a,b) * (a',b') = (a * a', b * b')
  negate (a, b) = (negate a, negate b)
  abs (a, b) = (abs a, abs b)
  signum (a, b) = (signum a, signum b)

liftPara :: Lens s t a b -> Para (Matrix p) (c, s) (c, t) a b
liftPara l = Para (fromLists [[]]) (p2 . l)

composePara :: forall p s1 s2 a b m n c q .
                                            Para p (s1, s2) (s1, s2) a b
                                        ->  Para q (c, a) (c, b) m n
                                        ->  Para (q, p) ((c, s1), s2) ((c, s1), s2) m n
composePara (Para p l1) (Para q l2) = Para cp cl
    where
        cp :: (q, p)
        cp = (q, p)

        cl :: Lens ((c, s1), s2) ((c, s1), s2) m n
        cl = assocL . (identityL `alongside` l1) . l2

(|.|) :: Para p (s1, s2) (s1, s2) a b
                        -> Para q (c, a) (c, b) m n
                        -> Para (q, p) ((c, s1), s2) ((c, s1), s2) m n
p |.| q = p `composePara` q
