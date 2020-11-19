{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Read some text and attempt to make a chart.
module Chart.Any
  ( chartAny,
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
import Control.Lens
import qualified Data.HashMap.Strict as HashMap
import Data.List ((!!))
import NumHask.Prelude hiding (fold)

-- | Attempt to read some text and interpret it as data suitable for charting.
--
-- >>> let c = chartAny $ show $ (,) <$> (((\x -> sin (pi * x/40)) . fromIntegral <$> ([1..40] :: [Int]))) <*> (((\x -> cos (pi * x/40)) . fromIntegral <$> ([1..40] :: [Int])))
-- >>> writeFile "other/chartany.svg" $ either id id c
--
-- ![chartAny Example](other/chartany.svg)
chartAny :: Text -> Either Text Text
chartAny t =
  maybe (Left "<html>bad read</html>") Right . head . rights $
    [ -- single list
      tryChart
        t
        ( \xs ->
            bool
              (let (h, c) = barChart defaultBarOptions (BarData [xs] Nothing Nothing) in chartSvg (mempty & #hudOptions .~ h & #chartList .~ c))
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
      -- just text FIXME: doesn't parse for escaped characters
      tryChart ("\"" <> t <> "\"") (\x -> chartSvgDefault [Chart (TextA defaultTextStyle ["\"" <> x <> "\""]) [zero]])
    ]

-- | Attempt to read chart data and write to file.
writeAny :: FilePath -> Text -> IO ()
writeAny f t = writeFile f $ either id id $ chartAny t

-- | Read some Text and try a chart with a particular shape.
tryChart :: (Read a) => Text -> (a -> Text) -> Either Text Text
tryChart t c = either (Left . pack) (Right . c) $ readEither (unpack t)

-- | Default chart for a double list.
chartList2 :: [[Double]] -> Text
chartList2 xss
  | (length xss < 4) && (length (xss !! 0) < 10) = anyBarChart xss
  -- square
  | all (length xss ==) (length <$> xss) =
    anySurfaceChart xss
  | otherwise = anyLineChart xss

-- | Default text chart.
chartText1 :: [Text] -> Text
chartText1 xs
  | (length xs < 20) = anyTextChart [xs]
  -- word salad
  | otherwise = anySingleNamedBarChart (second fromIntegral <$> HashMap.toList mapCount)
  where
    mapCount = foldl' (\m x -> HashMap.insertWith (+) x (1 :: Int) m) HashMap.empty xs

-- | Bar chart for a double list.
anyBarChart :: [[Double]] -> Text
anyBarChart xss = chartSvg (mempty & #hudOptions .~ h & #chartList .~ c)
  where
    (h, c) =
      barChart
        defaultBarOptions
        ( BarData
            xss
            (Just (("row " <>) . show <$> take nx [(0 :: Int) ..]))
            (Just (("col " <>) . show <$> take ny [(0 :: Int) ..]))
        )
      where
        nx = length xss
        ny = maximum (length <$> xss)

-- | Bar chart for a labelled list.
anySingleNamedBarChart :: [(Text, Double)] -> Text
anySingleNamedBarChart xs = chartSvg (mempty & #hudOptions .~ h & #chartList .~ c)
  where
    (h, c) =
      barChart
        defaultBarOptions
        ( BarData
            [snd <$> xs]
            (Just (fst <$> xs))
            Nothing
        )

-- | Default line chart for a double list
anyLineChart :: [[Double]] -> Text
anyLineChart xss =
  chartSvgHud (stdLineChart 0.02 palette1 xss)

-- | Default scatter chart for paired data
anyScatterChart :: [[(Double, Double)]] -> Text
anyScatterChart xss =
  chartSvgHud (scatterChart (fmap (fmap (uncurry Point)) xss))

-- | Default text chart for double list.
anyTextChart :: [[Text]] -> Text
anyTextChart xss =
  chartSvgHud (tableChart xss)

-- | Default pixel chart for double list.
anySurfaceChart :: [[Double]] -> Text
anySurfaceChart xss = chartSvg (ChartSvg defaultSvgOptions (anySurfaceHud nx ny) h c)
  where
    (c, h) =
      surfacefl
        (\(Point x y) -> ((xss !! (floor x)) !! (floor y)))
        (SurfaceOptions defaultSurfaceStyle (Point nx ny) (Rect 0 (fromIntegral nx :: Double) 0 (fromIntegral ny)))
        (defaultSurfaceLegendOptions "square")
    nx = length xss
    ny = length (xss !! 0)

anySurfaceHud :: Int -> Int -> HudOptions
anySurfaceHud nx ny =
  defaultHudOptions
    & #hudAxes
      .~ [ defaultAxisOptions
             & #atick . #tstyle .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labelsy)
             & #place .~ PlaceLeft,
           defaultAxisOptions
             & #atick . #tstyle .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labelsx)
             & #place .~ PlaceBottom
         ]
  where
    labelsx = show <$> [0 .. (nx - 1)]
    labelsy = show <$> [0 .. (ny - 1)]
