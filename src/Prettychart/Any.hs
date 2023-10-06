{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Read some text and attempt to make a chart.
module Prettychart.Any
  ( anyChart,
    anyWrite,
    tryChart,
    anyList1,
    anyList2,
    anyTuple2,
    anySingleNamedBarChart,
    anyBar2,
    anyLineChart,
    anySurfaceChart,
  )
where

import Chart
import Data.Either (rights)
import Data.Maybe
import Data.Text (Text, pack, unpack)
import Optics.Core
import Prettychart.Charts
import Text.Read (readEither)

-- $setup
--
-- >>> :set -Wno-type-defaults
-- >>> import Chart
-- >>> import Prettychart.Any
-- >>> import Data.Text (unpack)

-- | Attempt to read some text and interpret it as data suitable for charting.
--
-- In the example below, 'anyChart' determines that the input text is of type [(Double, Double)] and renders a scatter chart of the data.
--
-- >>> unknownData = (,) <$> (((\x -> sin (pi * x/40)) . fromIntegral <$> ([1..40] :: [Int]))) <*> (((\x -> cos (pi * x/40)) . fromIntegral <$> ([1..40] :: [Int])))
-- >>> let c = anyChart $ show $ unknownData
-- >>> writeFile "other/anychart.svg" $ either id (unpack . renderChartOptions) c
--
-- ![anyChart Example](other/anychart.svg)
anyChart :: String -> Either String ChartOptions
anyChart t =
  maybe (Left "<html>bad read</html>") Right . listToMaybe . rights $
    [ -- single list
      tryChart t anyList1,
      -- double list
      tryChart t anyList2,
      -- single tuple list
      tryChart t (\x -> anyTuple2 [x]),
      -- double tuple list
      tryChart t anyTuple2,
      -- (Text,Double) single list
      tryChart t anySingleNamedBarChart
    ]

-- | Attempt to read chart data and write to file.
anyWrite :: FilePath -> String -> IO ()
anyWrite f t = writeFile f $ either id (unpack . renderChartOptions) $ anyChart t

-- | Read a String and try a chart with a particular shape.
tryChart :: (Read a) => String -> (a -> ChartOptions) -> Either String ChartOptions
tryChart t c = c <$> readEither t

-- | Default chart for a single list.
anyList1 :: [Double] -> ChartOptions
anyList1 xs
  | length xs > 1000 = histChart (unsafeSpace1 xs) 20 xs
  | length xs > 10 = anyLineChart [xs]
  | otherwise = barChart defaultBarOptions (BarData [xs] [] [])

-- | Default chart for a double list.
anyList2 :: [[Double]] -> ChartOptions
anyList2 xss
  | (length xss < 4) && (length (head xss) < 10) = anyBar2 xss
  -- square
  | all (length xss ==) (length <$> xss) =
      anySurfaceChart xss
  | otherwise = anyLineChart xss

-- | Bar chart for a labelled list.
anySingleNamedBarChart :: [(Text, Double)] -> ChartOptions
anySingleNamedBarChart xs =
  barChart
    defaultBarOptions
    ( BarData
        [snd <$> xs]
        (fst <$> xs)
        []
    )

-- | Bar chart for a double list.
anyBar2 :: [[Double]] -> ChartOptions
anyBar2 xss =
  barChart
    defaultBarOptions
    ( BarData
        xss
        (pack . ("row " <>) . show <$> take nrows [(0 :: Int) ..])
        (pack . ("col " <>) . show <$> take ncols [(0 :: Int) ..])
    )
  where
    ncols = length xss
    nrows = maximum (length <$> xss)

-- | Multiple line chart.
anyLineChart :: [[Double]] -> ChartOptions
anyLineChart xss =
  mempty
    & #hudOptions .~ defaultHudOptions
    & #charts .~ unnamed (zipWith (\c xs -> simpleLineChart 0.02 (palette1 c) xs) [0 ..] xss)

-- | Default scatter chart for paired data
anyTuple2 :: [[(Double, Double)]] -> ChartOptions
anyTuple2 xss =
  mempty
    & #hudOptions .~ defaultHudOptions
    & #charts .~ unnamed (scatterChart (fmap (fmap (uncurry Point)) xss))

-- | Default pixel chart for double list.
anySurfaceChart :: [[Double]] -> ChartOptions
anySurfaceChart xss = mempty & #charts .~ ct
  where
    ct = runHud (aspect 1) h0 (unnamed c)
    (h0, _) = toHuds (anySurfaceHud nrows ncols) (FixedAspect 1) gr
    gr = Rect 0 (fromIntegral nrows :: Double) 0 (fromIntegral ncols)
    (c, _) =
      surfacef
        (\(Point x y) -> (xss' !! floor x) !! floor y)
        (SurfaceOptions defaultSurfaceStyle (Point nrows ncols) gr)
    -- (defaultSurfaceLegendOptions dark "")
    nrows = rows xss
    ncols = length xss
    xss' = appendZeros xss

-- | Number of rows
rows :: [[Double]] -> Int
rows xs = maximum $ (0 :) $ length <$> xs

appendZeros :: [[Double]] -> [[Double]]
appendZeros xs =
  ( \x ->
      take
        (rows xs)
        (x <> repeat 0)
  )
    <$> xs

anySurfaceHud :: Int -> Int -> HudOptions
anySurfaceHud nx ny =
  defaultHudOptions
    & #axes
      .~ [ ( 5,
             defaultYAxisOptions
               & #ticks % #style .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labelsy)
           ),
           ( 5,
             defaultXAxisOptions
               & #ticks % #style .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labelsx)
           )
         ]
  where
    labelsx = pack . show <$> [0 .. (nx - 1)]
    labelsy = pack . show <$> [0 .. (ny - 1)]
