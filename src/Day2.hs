{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid (Sum(..))
import Data.Bifunctor (bimap)
import Data.Foldable (foldl')

allOccurences :: Text -> Map Char Int
allOccurences = T.foldl' go mempty
 where
  go acc c = M.insertWith (+) c 1 acc

-- Using a pair of Ints that are 1 or 0 here instead of booleans just to make things a bit simpler
has2Or3 :: Map Char Int -> (Int, Int)
has2Or3 = foldl' go (0, 0)
 where
  go (y, z) x | x == 2    = (1, z)
              | x == 3    = (y, 1)
              | otherwise = (y, z)

solve1 :: [Text] -> Int
solve1 = getSum . f . foldMap (bimap Sum Sum . has2Or3 . allOccurences)
 where
  f (x, y) = x * y

tests :: [Bool]
tests =
  [ solve1 ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"] == 12
  ]

solveIO :: ([Text] -> Int) -> IO Int
solveIO f = f . T.lines <$> T.readFile "./inputs/Day2.txt"
