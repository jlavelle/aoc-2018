{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day4 where

import qualified Data.Attoparsec.Text as P
import Data.Attoparsec.Text (Parser)
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Time as Time
import Control.Applicative ((<|>), liftA2, liftA3)
import Data.Functor (($>))
import Data.List (unfoldr, sortBy)

data Event
  = BeginShift Int
  | FallAsleep
  | WakeUp
  deriving Show

data LogEntry = LogEntry
  { datetime :: Time.UTCTime
  , event    :: Event
  } deriving Show

sortByDT :: [LogEntry] -> [LogEntry]
sortByDT = sortBy (\a b -> datetime a `compare` datetime b)

parseEntry :: Parser LogEntry
parseEntry = liftA2 LogEntry (parseDateTime <* P.space) parseEvent <* (P.endOfLine <|> P.endOfInput)
 where
  parseDateTime = P.char '[' *> liftA2 Time.UTCTime (parseDate <* P.space) parseTime <* P.char ']'
   where
    parseDate = liftA3 Time.fromGregorian (P.decimal <* P.char '-') (P.decimal <* P.char '-') P.decimal
    parseTime = liftA2 toDiffTime (P.decimal <* P.anyChar) P.decimal
     where
      toDiffTime h m = Time.secondsToDiffTime $ h * 60^2 + m * 60
  parseEvent = parseFallAsleep <|> parseWakeUp <|> parseBeginShift
   where
    parseFallAsleep = P.string "falls asleep" $> FallAsleep
    parseWakeUp     = P.string "wakes up" $> WakeUp
    parseBeginShift = BeginShift <$> (P.string "Guard #" *> P.decimal <* P.takeTill P.isEndOfLine)

parse :: Text -> Either String [LogEntry]
parse = P.parseOnly (P.many1 parseEntry)

readInput :: IO [LogEntry]
readInput = either error id . parse <$> T.readFile "./inputs/Day4.txt"
