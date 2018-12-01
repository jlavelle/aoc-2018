module Day1 where

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Control.Lens (ala)
import Data.Monoid (Sum(..))
import qualified Data.Set as Set
import Data.Functor.Foldable (hylo)
import Data.Either (either)
import Data.Maybe (fromJust)

solve1 :: [Int] -> Int
solve1 = sum

solve2 :: [Int] -> Maybe Int
solve2 xs = hylo (either id id) coalg (cycle xs, 0, Set.singleton 0)
 where
  coalg ([], _, _) = Left Nothing
  coalg ((x:xs), acc, s) | Set.member acc' s = Left $ Just acc'
                         | otherwise         = Right (xs, acc', Set.insert acc' s)
   where
    acc' = x + acc

solve2' :: [Int] -> Int
solve2' = fromJust . solve2

parse :: Text -> Either String [Int]
parse = traverse (fmap fst . T.signed T.decimal) . T.lines

solveIO :: ([Int] -> Int) -> IO Int
solveIO f = fmap (f . either error id . parse) (T.readFile "./inputs/Day1.txt")

tests :: [Bool]
tests =
  [ solve2' [1, -1] == 0
  , solve2' [3, 3, 4, -2, -4] == 10
  ]