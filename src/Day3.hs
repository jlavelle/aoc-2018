module Day3 where

import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Map as M
import Data.Map (Map)
import Control.Applicative (liftA2, liftA3, (<|>))

data Rect = Rect
  { rectId :: Int
  , coords :: (Int, Int)
  , area   :: (Int, Int)
  } deriving Show

solve1 :: [Rect] -> Int
solve1 = M.size . M.filter (> 1) . M.unionsWith (+) . fmap rectCovers

rectCovers :: Rect -> Map (Int, Int) Int
rectCovers (Rect _ (x, y) (w, h)) = M.fromList cov
 where
  cov = [ ((x', y'), 1) | x' <- [x..x + w - 1], y' <- [y..y + h - 1] ]

parseRect :: Parser Rect
parseRect = liftA3 Rect parseId parseCoords parseArea <* (P.endOfLine <|> P.endOfInput)
 where
  parseId     = P.anyChar *> P.decimal <* P.space
  parseCoords = P.anyChar *> P.space *> liftA2 (,) (P.decimal <* P.char ',') P.decimal
  parseArea   = P.anyChar *> P.space *> liftA2 (,) (P.decimal <* P.char 'x') P.decimal

readInput :: IO [Rect]
readInput = either error id . P.parseOnly (P.many1 parseRect) <$> T.readFile "./inputs/Day3.txt"

tests :: [Bool]
tests =
  [ solve1 [ Rect 1 (1, 3) (4, 4)
           , Rect 2 (3, 1) (4, 4)
           , Rect 3 (5, 5) (2, 2)
           ] == 4
  ]
