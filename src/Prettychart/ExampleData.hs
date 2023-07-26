{-# LANGUAGE TemplateHaskell #-}

-- | Data for testing prettychart.
module Prettychart.ExampleData
  ( getReturns,
    taker,
  )
where

import Chart.FlatParse
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C
import Data.Time.Calendar
import FlatParse.Basic (Parser, Result (OK), char, runParser)

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
getReturns :: IO [(Day, Double)]
getReturns = do
  bs <- BS.readFile "other/returns.csv"
  pure $ runParserError dayReturnP <$> C.lines bs

-- | Take the last n of a list.
taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse
