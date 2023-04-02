{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RebindableSyntax #-}

module Chart.Various.Examples where

import FlatParse.Basic ( Parser, runParser, char, Result(OK) )
-- import Chart
import Chart.FlatParse
import qualified Data.ByteString as BS
-- import NumHask.Prelude
import Data.Time.Calendar
import qualified Data.ByteString.Char8 as C
import NumHask.Prelude hiding (id)

-- $setup
--
-- >>> import Chart
-- >>> import Chart.Any
-- >>> import Chart.Various
-- >>> import Chart.Various.Examples
-- >>> import Optics.Core
-- >>> import NumHask.Prelude hiding ((.), id)
-- >>> import Control.Category
-- >>> import FlatParse.Basic

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

getReturns :: IO [(Day,Double)]
getReturns = do
  bs <- BS.readFile "other/returns.csv"
  pure $ runParserError dayReturnP <$> C.lines bs

-- | Take the last n of a list.
taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

