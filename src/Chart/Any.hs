{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Read some text and attempt to make a chart.
module Chart.Any
  ( anyChart,
    anyWrite,
    tryChart,
    anyList1,
    anyList2,
    anyTuple2,
    anyText1,
    anyText2,
    anySingleNamedBarChart,
    anyBar2,
    anyLineChart,
    anySurfaceChart,
    anySurfaceHud,
  )
where

import Chart
import Chart.Various
import Optics.Core
import NumHask.Prelude hiding (fold, (.), id)
import Data.Text ( Text, unpack, pack )
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Read (readEither)
import Data.Bifunctor (second)
import Data.Either (rights)
import Control.Category

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> :set -Wno-type-defaults
-- >>> import Chart
-- >>> import Chart.Any
-- >>> import Data.Text (pack, Text)
-- >>> import qualified Data.Text as Text
-- >>> import qualified Data.Text.IO as Text
-- >>> import Chart.Various
-- >>> import Chart.Various.Examples
-- >>> import Optics.Core
-- >>> import NumHask.Prelude hiding ((.), id)
-- >>> import Control.Category
-- >>> import Data.Mealy
-- >>> r <- getReturns
-- >>> length r
-- 10897

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
      -- single list text
      tryChart t anyText1,
      -- double list text
      tryChart t anyText2,
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
            (pack . ("row " <>) . show <$> take nx [(0 :: Int) ..])
            (pack . ("col " <>) . show <$> take ny [(0 :: Int) ..])
        )
      where
        nx = length xss
        ny = maximum (length <$> xss)

anyLineChart :: [[Double]] -> ChartOptions
anyLineChart xss =
  mempty & #charts .~ unnamed (zipWith (\c xs -> simpleLineChart 0.02 (palette1 c) xs) [0..] xss)

-- | Default scatter chart for paired data
anyTuple2 :: [[(Double, Double)]] -> ChartOptions
anyTuple2 xss =
  mempty & #charts .~ unnamed (scatterChart (fmap (fmap (uncurry Point)) xss)) & #hudOptions .~ defaultHudOptions

-- | Default text chart.
anyText1 :: [Text] -> ChartOptions
anyText1 xs
  | length xs < 20 = anyText2 [xs]
  -- word salad
  | otherwise = anySingleNamedBarChart (second fromIntegral <$> Map.toList mapCount)
  where
    mapCount = foldl' (\m x -> Map.insertWith (+) x (1 :: Int) m) Map.empty xs

-- | Default text chart for double list.
anyText2 :: [[Text]] -> ChartOptions
anyText2 xss =
  mempty & #charts .~ unnamed (tableChart xss)

-- | Default pixel chart for double list.
anySurfaceChart :: [[Double]] -> ChartOptions
anySurfaceChart xss = ChartOptions defaultMarkupOptions (anySurfaceHud nx ny) ct
  where
    ct = runHud (aspect 1) h (unnamed c)

    (c, h) =
      surfacefl
        (\(Point x y) -> (xss !! floor x) !! floor y)
        (SurfaceOptions defaultSurfaceStyle (Point nx ny) (Rect 0 (fromIntegral nx :: Double) 0 (fromIntegral ny)))
        (defaultSurfaceLegendOptions dark "square")
    nx = length xss
    ny = length (head xss)

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
