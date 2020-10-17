{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Mealy scan 'Chart's.
module Chart.Mealy
  ( -- * Charts that take a Mealy scan function.
    scanChart,
    scanHud,
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
import Control.Lens
import qualified Data.List as List
import Data.List ((!!))
import Data.Mealy
import Data.Time (UTCTime (..))
import NumHask.Prelude hiding (fold)

-- | Take the last n of a list.
taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

-- | Mealy decay rate.
type Rate = Double

-- | Simple scan of a time series through a Mealy using a list of decay rates, with time dimension of [0..]
scanChart :: (Rate -> Mealy a Double) -> [Rate] -> Int -> [a] -> [Chart Double]
scanChart m rates d xs =
  zipWith
    (\s xs' -> Chart (LineA s) xs')
    (stdLines 0.003)
    (zipWith P (fromIntegral <$> [d ..]) <$> ((\r -> drop d $ scan (m r) xs) <$> rates))

-- | common line chart hud with rates as a legend
scanHud :: Double -> Text -> [Rate] -> HudOptions
scanHud w t rates =
  defaultHudOptions
    & #hudTitles .~ [defaultTitle t]
    & #hudLegend .~ Just (lineLegend w (("rate = " <>) . show <$> rates) palette1)

-- | fold over a scanned time series by rates
foldScanChart :: (Rate -> Mealy a b) -> (Rate -> Mealy b Double) -> [Rate] -> [a] -> [Chart Double]
foldScanChart scan' fold' rates xs =
  (: []) $
    Chart
      (LineA defaultLineStyle)
      (zipWith P rates ((\r -> fold (fold' r) $ scan (scan' r) xs) <$> rates))

zeroLineStyle :: LineStyle
zeroLineStyle = defaultLineStyle & #color .~ (palette1 !! 7) & #width .~ 0.002

-- | take a decaying scanner, a list of decay rates, and create linecharts from an [a]
scannerChart :: Int -> [Rate] -> (Double -> [a] -> [Double]) -> [a] -> [Chart Double]
scannerChart n rs rscan xs =
  addLineX 0 zeroLineStyle $
    stdLineChart 0.005 palette1 (tsrs n rs rscan xs)
  where
    tsrs n rs rscan xs = taker n . (`rscan` xs) <$> rs

-- | take a multi-decaying scanner, a decay rate, and create linecharts from an [a]
scannersChart :: Int -> Rate -> (Double -> [a] -> [[Double]]) -> [a] -> [Chart Double]
scannersChart n r rscan xs =
  addLineX 0 zeroLineStyle $
    stdLineChart 0.005 palette1 (tsrs n r rscan xs)
  where
    tsrs n r rscan xs = taker n <$> (List.transpose $ rscan r xs)

-- | a Hud for time series with a rates legend
tsRatesHud :: Text -> [Rate] -> [UTCTime] -> HudOptions
tsRatesHud title rs ds =
  defaultHudOptions
    & #hudTitles
    .~ [defaultTitle title & #style . #size .~ 0.08]
    & #hudLegend
    .~ Just (lineLegend 0.001 ((("rate: " <>) . show) <$> rs) palette1)
    & #hudAxes
    .~ tsAxes ds
