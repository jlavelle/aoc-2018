{-# LANGUAGE ViewPatterns #-}

module Day1 where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Control.Lens (ala)
import Data.Monoid (Sum(..))
import qualified Data.IntSet as Set
import Data.Functor.Foldable (hylo)
import Data.Either (either)
import Data.Maybe (fromJust)
import Data.Function (fix)

solve1 :: [Int] -> Int
solve1 = sum

solve2 :: [Int] -> Int
solve2 (cycle -> xs) = hylo (either id id) coalg (xs, 0, Set.singleton 0)

-- Denis Stoyanov's version using fix
solve2fix :: [Int] -> Int
solve2fix (cycle -> xs) = fix (\r f a -> either id (r f) (f a)) coalg (xs, 0, Set.singleton 0)

type Acc =  ([Int], Int, Set.IntSet)

coalg :: Acc -> Either Int Acc
coalg ([], _, _) = error "coalg requires an infinite list."
coalg ((x:xs), acc, s) | Set.member acc' s = Left acc'
                       | otherwise         = Right (xs, acc', Set.insert acc' s)
 where
  acc' = x + acc

-- Solution using general recursion
solve2Direct :: [Int] -> Int
solve2Direct (cycle -> ys) = go ys 0 (Set.singleton 0)
 where
  go [] _ _ = error "solve2Direct: Impossible"
  go (x:xs) acc s | Set.member acc' s = acc'
                  | otherwise         = go xs acc' (Set.insert acc' s)
   where
    acc' = x + acc

parse :: Text -> Either String [Int]
parse = traverse (fmap fst . T.signed T.decimal) . T.lines

solveIO :: ([Int] -> Int) -> IO Int
solveIO f = fmap f readInput

readInput :: IO [Int]
readInput = either error id . parse <$> T.readFile ("./inputs/Day1.txt")

tests :: [Bool]
tests =
  [ solve2 [1, -1] == 0
  , solve2 [3, 3, 4, -2, -4] == 10
  ]
