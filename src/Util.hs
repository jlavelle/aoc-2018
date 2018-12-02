module Util where

import Data.List (tails)

paired :: [a] -> [(a, a)]
paired xs = foldMap zip (drop 1 $ tails xs) xs

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
