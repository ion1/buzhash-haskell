module Data.BuzHash.Internal.ListAnd
  ( ListAnd (..)
  , listAndToList
  ) where

import Prelude hiding (foldr)

import Control.Applicative
import Data.Foldable
import Data.Traversable

data ListAnd final a = End !final | Cons !a (ListAnd final a)
  deriving (Eq, Ord, Read, Show)

instance Functor (ListAnd final) where
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  fmap _ (End fin)   = End fin

instance Foldable (ListAnd final) where
  foldr f z = go
    where
      go (Cons x xs) = f x (go xs)
      go (End _)     = z
  
instance Traversable (ListAnd final) where
  traverse f = go
    where
      go (Cons x xs) = Cons <$> f x <*> go xs
      go (End fin)   = pure (End fin)

listAndToList :: ListAnd final a -> ([a], final)
listAndToList (Cons x xs) =
  case listAndToList xs of
    ~(ys, fin) -> (x:ys, fin)
listAndToList (End fin) = ([], fin)
