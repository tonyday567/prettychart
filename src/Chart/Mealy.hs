{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use ?~" #-}
{-# LANGUAGE TupleSections #-}

-- | Mealy scan 'Chart's.
module Chart.Mealy
  ( -- * Charts that take a Mealy scan function.
    scanChart,
    foldScanChart,
    scannerChart,
    scannersChart,
    tsRatesHud,
    taker,
    Rate,
  )
where

import Chart
import Chart.Various
import Optics.Core
import qualified Data.List as List
import Data.Mealy
import Data.Time (UTCTime (..))
import NumHask.Prelude hiding (fold)
import Data.Text ( Text)

-- | Take the last n of a list.
taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

-- | Mealy decay rate.
type Rate = Double

-- | Simple scan of a time series through a Mealy using a list of decay rates, with time dimension of [0..]
scanChart :: (Rate -> Mealy a Double) -> [Rate] -> Int -> [a] -> [Chart]
scanChart m rates d xs =
  zipWith
    LineChart
    ((\c -> defaultLineStyle & #color .~ c & #size .~ 0.003) <$> (palette1 <$> [0..]))
    [zipWith Point (fromIntegral <$> [d ..]) <$> ((\r -> drop d $ scan (m r) xs) <$> rates)]

-- | fold over a scanned time series by rates
foldScanChart :: (Rate -> Mealy a b) -> (Rate -> Mealy b Double) -> [Rate] -> [a] -> [Chart]
foldScanChart scan' fold' rates xs =
  (: []) $
    LineChart
      defaultLineStyle
      [zipWith Point rates ((\r -> fold (fold' r) $ scan (scan' r) xs) <$> rates)]

zeroLineStyle :: LineStyle
zeroLineStyle = defaultLineStyle & #color .~ palette1 7 & #size .~ 0.002

-- | take a decaying scanner, a list of decay rates, and create linecharts from an [a]
scannerChart :: Int -> [Rate] -> (Double -> [a] -> [Double]) -> [a] -> [Chart]
scannerChart n rs rscan xs =
  addLineX 0 zeroLineStyle $
    zipWith (\c xs -> simpleLineChart 0.005 (palette1 c) xs) [0..] (tsrs n rs rscan xs)
  where
    tsrs n rs rscan xs = taker n . (`rscan` xs) <$> rs

-- | take a multi-decaying scanner, a decay rate, and create linecharts from an [a]
scannersChart :: Int -> Rate -> (Double -> [a] -> [[Double]]) -> [a] -> [Chart]
scannersChart n r rscan xs =
  addLineX 0 zeroLineStyle $
    zipWith (\c xs -> simpleLineChart 0.005 (palette1 c) xs) [0..] (tsrs n r rscan xs)
  where
    tsrs n r rscan xs = taker n <$> List.transpose (rscan r xs)

-- | a Hud for time series with a rates legend
tsRatesHud :: Text -> [UTCTime] -> HudOptions
tsRatesHud title ds =
  defaultHudOptions
    & #titles
    .~ [(5,defaultTitle title & #style % #size .~ 0.08)]
    & #axes .~ [(5,timeXAxis ds)]
