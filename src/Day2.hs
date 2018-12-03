{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Monoid (Sum(..))
import Data.Bifunctor (bimap)
import Data.Foldable (foldl')
import Data.List (partition, intersect)

import Util (paired, safeHead)

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

solve2 :: [String] -> Maybe String
solve2 = fmap (uncurry intersect) . safeHead . filter p . paired
 where
  p (x, y) = (==) 1 . length . snd . partition id $ zipWith (==) x y

solve2' :: [Text] -> Maybe String
solve2' = solve2 . fmap T.unpack

tests :: [Bool]
tests =
  [ solve1 ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"] == 12
  ]

solveIO :: ([Text] -> b) -> IO b
solveIO f = f <$> readInput

readInput :: IO [Text]
readInput = T.lines <$> T.readFile "./inputs/Day2.txt"
