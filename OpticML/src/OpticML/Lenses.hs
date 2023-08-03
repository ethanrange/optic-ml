{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module OpticML.Lenses
  ( Profunctor (..), Strong (..),
    UpStar (..),
    Optic, Lens, Lens',
    lens,
    rev, over, fwd,
    alongside
  )
where

import Control.Applicative (Const (..))
import Data.Bifunctor (bimap)

-- Define Profunctor and Strong / Cartesian profunctor classes

class (forall a. Functor (p a)) => Profunctor p where
  dimap :: (s -> a) -> (b -> t) -> p a b -> p s t

class Profunctor p => Strong p where
  first :: p a b -> p (a, c) (b, c)
  second :: p a b -> p (c, a) (c, b)

-- Define UpStar

newtype UpStar f a b = UpStar {unUpStar :: a -> f b} deriving stock Functor

instance Functor f => Profunctor (UpStar f) where
  dimap :: (s -> a) -> (b -> t) -> UpStar f a b -> UpStar f s t
  dimap ab cd (UpStar bfc) = UpStar (fmap cd . bfc . ab)
  {-# INLINE dimap #-}

instance (Functor f) => Strong (UpStar f) where
  first :: UpStar f a b -> UpStar f (a, c) (b, c)
  first (UpStar m) = UpStar $ \(a, c) -> (,c) <$> m a
  {-# INLINE first #-}

  second :: UpStar f a b -> UpStar f (c, a) (c, b)
  second (UpStar m) = UpStar $ \(c, a) -> (c,) <$> m a
  {-# INLINE second #-}

-- Define (Strong) Profunctor instances for (->)

instance Profunctor (->) where
  dimap :: (s -> a) -> (b -> t) -> (a -> b) -> (s -> t)
  dimap f g h = g . h . f
  {-# INLINE dimap #-}

instance Strong (->) where
  first :: (a -> b) -> (a, c) -> (b, c)
  first ab (a, c) = (ab a, c)
  {-# INLINE first #-}

  second :: (a -> b) -> (c, a) -> (c, b)
  second ab (c, a) = (c, ab a)
  {-# INLINE second #-}

-- Define Optic and Lens types

type Optic p s t a b = p a b -> p s t

type Lens s t a b = forall p. Strong p => Optic p s t a b

type Lens' s a = Lens s s a a

-- Smart constructor for lenses

lens :: (s -> a) -> ((b, s) -> t) -> Lens s t a b
lens f r = dimap (\s -> (f s, s)) r . first
{-# INLINE lens #-}

-- rev opt new orig = opt (const new) orig
rev :: Optic (->) s t a b -> (b, s) -> t
rev opt (new, orig) = opt (const new) orig
{-# INLINE rev #-}

-- over opt fun orig = opt fun orig
over :: Optic (->) s t a b -> (a -> b) -> s -> t
over = id
{-# INLINE over #-}

-- Coerce away newtypes of UpStar and Const functor
fwd :: Optic (UpStar (Const a)) s t a b -> (s -> a)
fwd opt = getConst . unUpStar (opt (UpStar Const))
{-# INLINE fwd #-}

-- alongside

alongside :: Lens s t a b -> Lens s' t' a' b' -> Lens (s, s') (t, t') (a, a') (b, b')
alongside l1 l2 = lens (bimap (fwd l1) (fwd l2)) u
    where 
      u ((b, b'), (s, s')) = (rev l1 (b, s), rev l2 (b', s'))
{-# INLINE alongside #-}