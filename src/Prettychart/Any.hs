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
    anySurfaceHud,
    )
where

import Chart
import Prettychart.Charts
import Optics.Core
import Data.Text ( Text, unpack, pack )
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Read (readEither)
import Data.Either (rights)
import Data.Maybe

-- $setup
--
-- >>> :set -Wno-type-defaults
-- >>> import Chart
-- >>> import Prettychart.Any
-- >>> import Data.Text (pack, Text)
-- >>> import qualified Data.Text as Text
-- >>> import qualified Data.Text.IO as Text

-- | Attempt to read some text and interpret it as data suitable for charting.
--
-- In the example below, chartAny determines that the input text is of type [(Double, Double)] and renders a scatter chart of the data.
--
-- >>> unknownData = (,) <$> (((\x -> sin (pi * x/40)) . fromIntegral <$> ([1..40] :: [Int]))) <*> (((\x -> cos (pi * x/40)) . fromIntegral <$> ([1..40] :: [Int])))
-- >>> let c = anyChart $ pack $ show $ (,) <$> (((\x -> sin (pi * x/40)) . fromIntegral <$> ([1..40] :: [Int]))) <*> (((\x -> cos (pi * x/40)) . fromIntegral <$> ([1..40] :: [Int])))
-- >>> Text.writeFile "other/chartany.svg" $ either id renderChartOptions c
--
-- ![chartAny Example](other/anychart.svg)
anyChart :: Text -> Either Text ChartOptions
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
anyWrite :: FilePath -> Text -> IO ()
anyWrite f t = Text.writeFile f $ either id renderChartOptions $ anyChart t

-- | Read some Text and try a chart with a particular shape.
tryChart :: (Read a) => Text -> (a -> ChartOptions) -> Either Text ChartOptions
tryChart t c = either (Left . Text.pack) (Right . c) $ readEither (unpack t)

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
anySingleNamedBarChart xs = barChart
        defaultBarOptions
        ( BarData
            [snd <$> xs]
            (fst <$> xs)
            []
        )

-- | Bar chart for a double list.
anyBar2 :: [[Double]] -> ChartOptions
anyBar2 xss = barChart
        defaultBarOptions
        ( BarData
            xss
            (pack . ("row " <>) . show <$> take nrows [(0 :: Int) ..])
            (pack . ("col " <>) . show <$> take ncols [(0 :: Int) ..])
        )
      where
        ncols = length xss
        nrows = maximum (length <$> xss)

anyLineChart :: [[Double]] -> ChartOptions
anyLineChart xss =
  mempty &
  #hudOptions .~ defaultHudOptions &
  #charts .~ unnamed (zipWith (\c xs -> simpleLineChart 0.02 (palette1 c) xs) [0..] xss)

-- | Default scatter chart for paired data
anyTuple2 :: [[(Double, Double)]] -> ChartOptions
anyTuple2 xss =
  mempty &
  #hudOptions .~ defaultHudOptions &
  #charts .~ unnamed (scatterChart (fmap (fmap (uncurry Point)) xss))

-- | Default pixel chart for double list.
anySurfaceChart :: [[Double]] -> ChartOptions
anySurfaceChart xss = mempty & #charts .~ ct
  where
    ct = runHud (aspect 1) (h0 <> h1) (unnamed c)
    (h0, _) = toHuds (anySurfaceHud nrows ncols) gr
    gr = Rect 0 (fromIntegral nrows :: Double) 0 (fromIntegral ncols)
    (c, h1) =
      surfacefl
        (\(Point x y) -> (xss' !! floor x) !! floor y)
        (SurfaceOptions defaultSurfaceStyle (Point nrows ncols) gr)
        (defaultSurfaceLegendOptions dark "")
    nrows = rows xss
    ncols = length xss
    xss' = appendZeros xss

-- | Number of rows
rows :: [[Double]] -> Int
rows xs = maximum $ (0:) $ length <$> xs

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
      .~ [ (5, defaultAxisOptions
             & #ticks % #style .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labelsy)
             & #place .~ PlaceLeft),
           (5, defaultAxisOptions
             & #ticks % #style .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labelsx)
             & #place .~ PlaceBottom)
         ]
  where
    labelsx = pack . show <$> [0 .. (nx - 1)]
    labelsy = pack . show <$> [0 .. (ny - 1)]

