module Day3 where

import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Map as M
import Data.Map (Map)
import Control.Applicative (liftA2, liftA3, (<|>))
import Data.Maybe (isJust)
import Data.Foldable (traverse_)
import Util (safeHead)

data Rect = Rect
  { rectId :: Int
  , coords :: (Int, Int)
  , area   :: (Int, Int)
  } deriving Show

solve1 :: [Rect] -> Int
solve1 = M.size . M.filter (> 1) . fabricWith (const 1) (+)

-- TODO: Needs to be optimized!
solve2 :: [Rect] -> Maybe Int
solve2 rs = fmap rectId . safeHead $ filter (flip containsRect fabric) rs
 where
  fabric = fmap head . M.filter ((==) 1 . length) $ fabricWith (pure . rectId) (<>) rs

fabricWith :: (Rect -> a) -> (a -> a -> a) -> [Rect] -> Map (Int, Int) a
fabricWith f g = M.unionsWith g . fmap h
 where
  h r = M.fromList . flip zip (repeat $ f r) $ rectCovers r

containsRect :: Rect -> Map (Int, Int) a -> Bool
containsRect r m = isJust $ traverse_ (flip M.lookup m) (rectCovers r)

rectCovers :: Rect -> [(Int, Int)]
rectCovers (Rect _ (x, y) (w, h)) = [(x', y') | x' <- [x..x + w - 1], y' <- [y..y + h - 1]]

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
