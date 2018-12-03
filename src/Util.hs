{-# LANGUAGE TupleSections #-}

module Util where

import Data.Functor.Foldable (para, ListF(..))

paired :: [a] -> [(a, a)]
paired = para alg
 where
  alg Nil = []
  alg (Cons x (xs, r)) = fmap (x,) xs <> r

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
