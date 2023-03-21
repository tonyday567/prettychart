{-# LANGUAGE TemplateHaskell #-}

module Chart.Various.Examples where

import FlatParse.Basic
import Chart
import qualified Data.ByteString as BS
-- import NumHask.Prelude
import Data.Time.Calendar
import qualified Data.ByteString.Char8 as C
import Data.Mealy
import qualified Data.Map.Strict as Map
import NumHask.Prelude hiding (id)
import Control.Category (id)

-- | Day parser, consumes separator
--
-- >>> runParser dayP "2020-07-28"
-- OK 2020-07-28 ""
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
  _ -> undefined

dayReturnP :: Parser e (Day, Double)
dayReturnP = (,) <$> dayP <*> ($(char ',') *> signed double)

getReturns :: IO [(Day,Double)]
getReturns = do
  bs <- BS.readFile "other/returns.csv"
  pure $ runParserError dayReturnP <$> C.lines bs

-- difference mealy
diff1 :: (a -> a -> b) -> a -> Mealy a b
diff1 f a0 = f <$> id <*> delay1 a0

count :: (Ord a) => [a] -> Map.Map a Int
count = foldl' (\m k -> Map.insertWith (+) k 1 m) Map.empty
