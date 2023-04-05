{-# LANGUAGE TemplateHaskell #-}

-- | Data for testing prettychart.
--
module Prettychart.ExampleData
  ( getReturns,
    taker,
  ) where

import FlatParse.Basic ( Parser, runParser, char, Result(OK) )
import Chart.FlatParse
import qualified Data.ByteString as BS
import Data.Time.Calendar
import qualified Data.ByteString.Char8 as C

dayP :: Parser e Day
dayP = do
  y <- int
  _ <- $(char '-')
  m <- int
  _ <- $(char '-')
  d <- int
  pure $ fromGregorian (fromIntegral y) m d

runParserError :: Parser e a -> BS.ByteString -> a
runParserError p bs = case runParser p bs of
  OK r _ -> r
  _ -> error "uncaught flatparse error"

dayReturnP :: Parser e (Day, Double)
dayReturnP = (,) <$> dayP <*> ($(char ',') *> signed double)

-- | Read and parse example data, which is daily stock market returns since 1980.
getReturns :: IO [(Day,Double)]
getReturns = do
  bs <- BS.readFile "other/returns.csv"
  pure $ runParserError dayReturnP <$> C.lines bs

-- | Take the last n of a list.
taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

