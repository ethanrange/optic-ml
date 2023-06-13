{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpticML.Parametric
  ( Para(..), Para',
    liftPara,
    composePara, (|.|)
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

(|.|) :: Para p (s1, s2) (s1, s2) a b
    -> Para q (c, a) (c, b) m n
    -> Para (q, p) ((c, s1), s2) ((c, s1), s2) m n
p1 |.| p2 = p1 `composePara` p2
