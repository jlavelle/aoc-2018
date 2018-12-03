module Main where

import Criterion.Main

import qualified Day1
import qualified Day2
import qualified Util
import qualified Data.Text as T
import qualified Data.Sequence as Seq
import Control.Lens ((<&>))

main :: IO ()
main = do
  i  <- Day1.readInput
  i2 <- Day2.readInput <&> fmap T.unpack
  defaultMain
    [ bgroup "Day1" [ bench "hylo" $ whnf Day1.solve2 i
                    , bench "fix"  $ whnf Day1.solve2fix i
                    , bench "gen"  $ whnf Day1.solve2Direct i
                    ]
    , bgroup "Day2" [ bench "part2" $ whnf Day2.solve2 i2 ]
    ]
