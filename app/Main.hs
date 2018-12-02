module Main where

import Criterion.Main

import Day1

main :: IO ()
main = do
  i <- readInput
  defaultMain
    [ bgroup "Day1" [ bench "hylo" $ whnf solve2 i
                    , bench "fix"  $ whnf solve2fix i
                    , bench "gen"  $ whnf solve2Direct i
                    ]
    ]
