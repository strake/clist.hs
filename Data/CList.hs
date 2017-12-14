{-# LANGUAGE StandaloneDeriving #-}

module Data.CList (module Data.Peano, CList (..), uncons, head, tail, init, last, reverse) where

import Prelude (Read, Show, fst, snd)

import Control.Applicative
import Control.Category.Unicode
import Data.Eq
import Data.Foldable
import Data.Functor
import Data.Monoid hiding ((<>))
import Data.Ord
import Data.Peano
import Data.Semigroup
import Data.Traversable
import Data.Typeable

infixr 5 :.

data CList n a where
    Nil :: CList Zero a
    (:.) :: a -> CList n a -> CList (Succ n) a

deriving instance (Eq   a) => Eq   (CList n a)
deriving instance (Ord  a) => Ord  (CList n a)
deriving instance (Show a) => Show (CList n a)
deriving instance Functor     (CList n)
deriving instance Foldable    (CList n)
deriving instance Traversable (CList n)
deriving instance Typeable CList

instance Semigroup a => Semigroup (CList n a) where
    Nil <> Nil = Nil
    (x:.xs) <> (y:.ys) = x<>y:.xs<>ys

instance (Semigroup a, Monoid a) => Monoid (CList Zero a) where
    mempty = Nil
    mappend = (<>)

instance (Semigroup a, Semigroup (CList n a),
          Monoid a, Monoid (CList n a)) => Monoid (CList (Succ n) a) where
    mempty = mempty:.mempty
    mappend = (<>)

instance Applicative (CList Zero) where
    pure x = Nil
    Nil <*> Nil = Nil

instance (Applicative (CList n)) => Applicative (CList (Succ n)) where
    pure x = x :. pure x
    f:.fs <*> x:.xs = f x :. (fs <*> xs)

uncons :: CList (Succ n) a -> (a, CList n a)
uncons (x:.xs) = (x, xs)

head :: CList (Succ n) a -> a
head = fst ∘ uncons

tail :: CList (Succ n) a -> CList n a
tail = snd ∘ uncons

init :: CList (Succ n) a -> CList n a
init (x:.Nil)       = Nil
init (x:.xs@(_:._)) = x:.init xs

last :: CList (Succ n) a -> a
last (x:.Nil) = x
last (x:.xs@(_:._)) = last xs

reverse :: CList n a -> CList n a
reverse Nil = Nil
reverse xs@(_:._) = liftA2 (:.) last (reverse ∘ init) xs
