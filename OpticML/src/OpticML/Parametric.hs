{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}

module OpticML.Parametric
  ( Para(..), Para',
    liftPara,
    composePara, (|.|),
    EmptyParam(..)
  )
where

import OpticML.Lenses (Lens, alongside)
import OpticML.LensImpl (p2, assocL, identityL)

data Para p s t a b = Para
    { params :: p
    , plens :: Lens s t a b
    }

type Para' p s a = Para p s s a a

data EmptyParam = EmptyParam deriving stock Show

instance (Num a, Num b) => Num (a,b) where
  fromInteger n   = (fromInteger n, fromInteger n)
  (a,b) + (a',b') = (a + a', b + b')
  (a,b) - (a',b') = (a - a', b - b')
  (a,b) * (a',b') = (a * a', b * b')
  negate (a, b) = (negate a, negate b)
  abs (a, b) = (abs a, abs b)
  signum (a, b) = (signum a, signum b)

instance Num EmptyParam where
  fromInteger _ = EmptyParam
  (+) _ _       = EmptyParam
  (-) _ _       = EmptyParam
  (*) _ _       = EmptyParam
  negate        = const EmptyParam
  abs           = const EmptyParam
  signum        = const EmptyParam

liftPara :: Lens s t a b -> Para EmptyParam (c, s) (c, t) a b
liftPara l = Para EmptyParam (p2 . l)

composePara :: forall p s a b m n q t . Para p (p, s) (p, t) a b
                                     -> Para q (q, a) (q, b) m n
                                     -> Para (q, p) ((q, p), s) ((q, p), t) m n
composePara (Para p l1) (Para q l2) = Para (q, p) cl
    where
        cl :: Lens ((q, p), s) ((q, p), t) m n
        cl = assocL . (identityL `alongside` l1) . l2

(|.|) :: Para p (p, s) (p, t) a b
      -> Para q (q, a) (q, b) m n
      -> Para (q, p) ((q, p), s) ((q, p), t) m n
p |.| q = p `composePara` q
