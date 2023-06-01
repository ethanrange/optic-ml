{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module OpticML.Lenses
  ( Profunctor (..), Strong (..),
    UpStar (..),
    Optic, Lens, Lens',
    lens,
    set, over, view,
    alongside,
    p1, p2,
  )
where

import Control.Applicative (Const (..))
import Data.Coerce (coerce)
import Data.Bifunctor (bimap)

-- Define Profunctor and Strong / Cartesian profunctor classes

class (forall a. Functor (p a)) => Profunctor p where
  dimap :: (s -> a) -> (b -> t) -> p a b -> p s t

class Profunctor p => Strong p where
  first :: p a b -> p (a, c) (b, c)
  second :: p a b -> p (c, a) (c, b)

-- Define UpStar

newtype UpStar f a b = UpStar {unUpStar :: a -> f b}

instance Functor f => Functor (UpStar f x) where
  fmap :: (a -> b) -> UpStar f x a -> UpStar f x b
  fmap k (UpStar f) = UpStar (fmap k . f)
  {-# INLINE fmap #-}

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
lens fwd rev = dimap (\s -> (fwd s, s)) rev . first
{-# INLINE lens #-}

-- set opt new orig = optic (const new) orig
set :: Optic (->) s t a b -> b -> s -> t
set optic = optic . const
{-# INLINE set #-}

-- over opt fun orig = opt fun orig
over :: Optic (->) s t a b -> (a -> b) -> s -> t
over = id
{-# INLINE over #-}

-- Coerce away newtypes of UpStar and Const functor
view :: Optic (UpStar (Const a)) s t a b -> s -> a
view opt = coerce (opt (UpStar Const))
{-# INLINE view #-}

-- alongside

alongside :: Lens s t a b -> Lens s' t' a' b' -> Lens (s, s') (t, t') (a, a') (b, b')
alongside l1 l2 = lens (bimap (view l1) (view l2)) u
    where 
      u ((b, b'), (s, s')) = (set l1 b s, set l2 b' s')

-- p1 and p2 lenses

p1 :: Lens (a, c) (b, c) a b
p1 = lens fst $ \(n, (_, b)) -> (n, b)
{-# INLINE p1 #-}

p2 :: Lens (x, a) (x, b) a b
p2 = lens snd $ \(n, (a, _)) -> (a, n)
{-# INLINE p2 #-}
