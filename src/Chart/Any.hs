{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Read some text and attempt to make a chart.
module Chart.Any
  ( -- chartAny,
    writeAny,
    tryChart,
    chartList2,
    chartText1,
    anyBarChart,
    anySingleNamedBarChart,
    anyLineChart,
    anyScatterChart,
    anyTextChart,
  )
where

import Chart
import Chart.Various
import Optics.Core
import NumHask.Prelude hiding (fold)
import Data.Text ( Text, unpack, pack )
-- import Data.Either
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Read (readEither)
import qualified Data.ByteString.Char8 as C
import Data.Bifunctor (second)
import Data.Either (rights)

chartToText :: ChartOptions -> Text
chartToText = pack . C.unpack . printChartOptions

-- | Attempt to read some text and interpret it as data suitable for charting.
--
-- >>> let c = chartAny $ show $ (,) <$> (((\x -> sin (pi * x/40)) . fromIntegral <$> ([1..40] :: [Int]))) <*> (((\x -> cos (pi * x/40)) . fromIntegral <$> ([1..40] :: [Int])))
-- >>> writeFile "other/chartany.svg" $ either id id c
--
-- ![chartAny Example](other/chartany.svg)
chartAny :: Text -> Either Text Text
chartAny t =
  maybe (Left "<html>bad read</html>") Right . listToMaybe . rights $
    [ -- single list
      tryChart
        t
        ( \xs ->
            bool
              (chartToText (barChart defaultBarOptions (BarData [xs] [] [])))
              (anyLineChart [xs])
              (length xs > 10)
        ),
      -- double list
      tryChart t chartList2,
      -- tuple
      tryChart t (\x -> anyScatterChart [x]),
      tryChart t anyScatterChart,
      -- text
      tryChart t anyTextChart,
      tryChart t chartText1,
      tryChart ("\"" <> t <> "\"") (\x -> chartToText (mempty & #charts .~ unnamed [TextChart defaultTextStyle [("\"" <> x <> "\"", zero)]]))
    ]

-- | Attempt to read chart data and write to file.
writeAny :: FilePath -> Text -> IO ()
writeAny f t = Text.writeFile f $ either id id $ chartAny t

-- | Read some Text and try a chart with a particular shape.
tryChart :: (Read a) => Text -> (a -> Text) -> Either Text Text
tryChart t c = either (Left . Text.pack) (Right . c) $ readEither (unpack t)

-- | Default chart for a double list.
chartList2 :: [[Double]] -> Text
chartList2 xss
  | (length xss < 4) && (length (head xss) < 10) = anyBarChart xss
  -- square
  | all (length xss ==) (length <$> xss) =
    anySurfaceChart xss
  | otherwise = anyLineChart xss

-- | Default text chart.
chartText1 :: [Text] -> Text
chartText1 xs
  | length xs < 20 = anyTextChart [xs]
  -- word salad
  | otherwise = anySingleNamedBarChart (second fromIntegral <$> Map.toList mapCount)
  where
    mapCount = foldl' (\m x -> Map.insertWith (+) x (1 :: Int) m) Map.empty xs

-- | Bar chart for a double list.
anyBarChart :: [[Double]] -> Text
anyBarChart xss = chartToText c
  where
    c =
      barChart
        defaultBarOptions
        ( BarData
            xss
            (pack . ("row " <>) . show <$> take nx [(0 :: Int) ..])
            (pack . ("col " <>) . show <$> take ny [(0 :: Int) ..])
        )
      where
        nx = length xss
        ny = maximum (length <$> xss)

-- | Bar chart for a labelled list.
anySingleNamedBarChart :: [(Text, Double)] -> Text
anySingleNamedBarChart xs = chartToText $ barChart
        defaultBarOptions
        ( BarData
            [snd <$> xs]
            (fst <$> xs)
            []
        )

-- | Default line chart for a double list
anyLineChart :: [[Double]] -> Text
anyLineChart xss =
  chartToText (mempty & #charts .~ unnamed (stdLineChart 0.02 (palette1 <$> [0..]) xss))

-- | Default scatter chart for paired data
anyScatterChart :: [[(Double, Double)]] -> Text
anyScatterChart xss =
  chartToText (mempty & #charts .~ unnamed (scatterChart (fmap (fmap (uncurry Point)) xss)))

-- | Default text chart for double list.
anyTextChart :: [[Text]] -> Text
anyTextChart xss =
  chartToText $ mempty & #charts .~ unnamed (tableChart xss)

-- | Default pixel chart for double list.
anySurfaceChart :: [[Double]] -> Text
anySurfaceChart xss = chartToText (ChartOptions defaultMarkupOptions (anySurfaceHud nx ny) ct)
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
