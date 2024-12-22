{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Various common (& pretty) chart patterns.
module Prettychart.Charts
  ( UtcAxisStyle (..),
    defaultUtcAxisStyle,
    utcAxis,
    DecileAxisStyle (..),
    defaultDecileAxisStyle,
    qsAxisStyle,
    decileAxis,
    DigitChartStyle (..),
    defaultDigitChartStyle,
    digitChart,
    UtcLineChartStyle (..),
    defaultUtcLineChartStyle,
    utcLineChart,
    CountChartStyle (..),
    defaultCountChartStyle,
    countChart,
    simpleRectChart,
    simpleLineChart,
    simpleScatterChart,
    xify,
    yify,
    titles3,
    histChart,
    hhistChart,
    hhistCharts,
    scatterChart,
    blendMidLineStyles,
    quantileNames,
    quantileChart,
    quantileHistChart,
    digitSurfaceChart,
  )
where

import Chart hiding (abs)
import Data.Bifunctor
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Time (UTCTime (..))
import NumHask.Space
import Optics.Core
import Data.Bool
import GHC.Generics
import Control.Category ((>>>))
import Data.List qualified as List

-- $setup
--
-- >>> :set -Wno-type-defaults
-- >>> import Chart
-- >>> import Prettychart.Charts
-- >>> import Data.Text (pack, Text)
-- >>> import qualified Data.Text as Text
-- >>> import qualified Data.Text.IO as Text


data UtcAxisStyle =
  UtcAxisStyle {
    cont :: Bool,
    posd :: PosDiscontinuous,
    utcFormat :: Maybe Text,
    nTicks :: Int
  } deriving (Generic)

defaultUtcAxisStyle :: UtcAxisStyle
defaultUtcAxisStyle = UtcAxisStyle True PosInnerOnly (Just "%b %y") 8

-- | Create a hud that has time as the x-axis based on supplied UTCTime list.
utcAxis :: UtcAxisStyle -> [UTCTime] -> AxisOptions
utcAxis s ds =
  defaultXAxisOptions
    & #ticks
    % #tick
    .~ TickPlaced
      (bool
       (fmap (first fromIntegral) $ fst $ placedTimeLabelDiscontinuous posd utcFormat nTicks ds)
       (first (* fromIntegral (length ds - 1)) <$> placedTimeLabelContinuous posd utcFormat nTicks (unsafeSpace1 ds))
       cont)
  where
    cont = view #cont s
    posd = view #posd s
    utcFormat = view #utcFormat s
    nTicks = view #nTicks s

data DecileAxisStyle =
  DecileAxisStyle {
    size :: Double,
    labels :: [Text]
  } deriving (Generic)

defaultDecileAxisStyle :: DecileAxisStyle
defaultDecileAxisStyle = DecileAxisStyle 0.04 []

qsAxisStyle :: [Double] -> DecileAxisStyle
qsAxisStyle qs = defaultDecileAxisStyle & set #labels (quantileNames qs)

decileAxis :: DecileAxisStyle -> AxisOptions
decileAxis s =
  defaultYAxisOptions
    & set (#ticks % #tick) (TickPlaced (zip ((+ 0.5) <$> [0 ..]) (view #labels s)))
    & set (#ticks % #lineTick) Nothing
    & set (#ticks % #textTick %? #style % #size) (view #size s)

data DigitChartStyle =
  DigitChartStyle {
    utcAxisStyle :: Maybe UtcAxisStyle,
    decileAxisStyle :: Maybe DecileAxisStyle,
    glyphStyle :: Style,
    hasLegend :: Bool
  } deriving (Generic)

defaultDigitChartStyle :: DigitChartStyle
defaultDigitChartStyle = DigitChartStyle (Just defaultUtcAxisStyle) Nothing (defaultGlyphStyle & set #size 0.01) True

-- | A chart drawing a (quantiled or digitized) time series
digitChart ::
  DigitChartStyle ->
  [UTCTime] ->
  [Int] ->
  ChartOptions
digitChart s utcs xs =
  mempty & #chartTree .~ unnamed [c] & #hudOptions .~ hudOptions
  where
    xaxis = view #utcAxisStyle s & fmap ((\c -> utcAxis c utcs)) & maybeToList
    yaxis = view #decileAxisStyle s & fmap decileAxis & maybeToList
    hudOptions =
      defaultHudOptions
        & set #axes (fmap (Priority 5) (xaxis <> yaxis))
    c = GlyphChart (view #glyphStyle s) (xify (fromIntegral <$> xs))

data UtcLineChartStyle = UtcLineChartStyle
  { lineStyle :: Style,
    utcAxisStyle :: Maybe UtcAxisStyle,
    yAxisStyle :: Maybe AxisOptions,
    legendStyle :: Maybe LegendOptions
  } deriving (Generic)

defaultUtcLineChartStyle :: UtcLineChartStyle
defaultUtcLineChartStyle = UtcLineChartStyle (defaultLineStyle & set #size 0.005) (Just defaultUtcAxisStyle) (Just $ defaultYAxisOptions & set #place PlaceLeft & set (#ticks % #tick) (TickRound (FormatN FSPercent (Just 2) 4 True True) 6 TickExtend)) (Just (defaultLegendOptions & set #place PlaceBottom & set #frame (Just $ border 0.01 light) & set (#textStyle % #size) 0.2 & set #anchorTo HudStyleSection))

utcLineChart :: UtcLineChartStyle -> [Text] -> [(UTCTime, [Double])] -> ChartOptions
utcLineChart s labels xs = mempty & #chartTree .~ named "day" cs & #hudOptions .~ h
  where
    cs = zipWith (\c xs' -> LineChart (view #lineStyle s & set #color c) [xify xs']) ((\x -> paletteO x 0.7) <$> [1, 2, 6, 7, 5, 3, 4, 0]) (List.transpose $ snd <$> xs)
    xaxis = view #utcAxisStyle s & fmap ((\c -> utcAxis c (fmap fst xs))) & maybeToList
    yaxis = view #yAxisStyle s & maybeToList
    leg = view #legendStyle s & fmap (set #legendCharts (zipWith (\t c -> (t, [c])) labels cs) >>> Priority 12) & maybeToList
    h = defaultHudOptions & set #axes (fmap (Priority 5) (xaxis <> yaxis)) & set #legends leg

-- | Chart template for quantiles.
quantileChart ::
  [Text] ->
  [Style] ->
  [[Double]] ->
  ChartOptions
quantileChart names ls xs = mempty & #hudOptions .~ h & #chartTree .~ unnamed c
  where
    h =
      defaultHudOptions
        & ( #legends
              .~ [ Priority 10 $
                     defaultLegendOptions
                       & #textStyle
                       % #size
                       .~ 0.1
                       & #vgap
                       .~ 0.05
                       & #innerPad
                       .~ 0.2
                       & #place
                       .~ PlaceRight
                       & #legendCharts
                       .~ zip names ((: []) <$> c)
                 ]
          )
    c =
      zipWith
        (\l x -> LineChart l [x])
        ls
        (zipWith Point [0 ..] <$> xs)

-- | convert from [a] to [Point a], by adding the index as the x axis
--
-- >>> xify [1..3]
-- [Point 0.0 1.0,Point 1.0 2.0,Point 2.0 3.0]
xify :: [Double] -> [Point Double]
xify ys =
  zipWith Point [0 ..] ys

-- | convert from [a] to [Point a], by adding the index as the y axis
--
-- >>> yify [1..3]
-- [Point 1.0 0.0,Point 2.0 1.0,Point 3.0 2.0]
yify :: [Double] -> [Point Double]
yify xs =
  zipWith Point xs [0 ..]

-- | interpret a [Double] as a line with x coordinates of [0..]
simpleLineChart :: Double -> Colour -> [Double] -> Chart
simpleLineChart w c xs =
  LineChart
    (defaultLineStyle & #color .~ c & #size .~ w)
    [xify xs]

-- | interpret a [Double] as a scatter chart with x coordinates of [0..]
simpleScatterChart :: Double -> Colour -> [Double] -> Chart
simpleScatterChart w c xs =
  GlyphChart
    (defaultGlyphStyle & #color .~ c & #size .~ w)
    (xify xs)

-- | common pattern of chart title, x-axis title and y-axis title
titles3 :: Double -> (Text, Text, Text) -> [Priority TitleOptions]
titles3 p (t, x, y) =
  [ Priority p (defaultTitleOptions t & #style % #size .~ 0.08),
    Priority p (defaultTitleOptions x & #place .~ PlaceBottom & #style % #size .~ 0.05),
    Priority p (defaultTitleOptions y & #place .~ PlaceLeft & #style % #size .~ 0.05)
  ]

data CountChartStyle = CountChartStyle {
  title :: Maybe Text,
  titleColour :: Colour,
  legendStyle :: Maybe LegendOptions
} deriving (Generic)

defaultCountChartStyle :: CountChartStyle
defaultCountChartStyle = CountChartStyle (Just "count") (paletteO 10 0.7) (Just (defaultLegendOptions & set #place PlaceRight & set #frame (Just $ border 0.01 light) & set (#textStyle % #size) 0.2 & set #anchorTo HudStyleSection))

countChart :: CountChartStyle -> [Text] -> [Int] -> ChartOptions
countChart s ls cs =
  barChart defaultBarOptions (BarData (List.transpose [fromIntegral <$> cs]) [] ls) & set (#hudOptions % #axes) [] & set (#chartTree % charts' % each % #chartStyle % #borderSize) 0 & set (#hudOptions % #legends % ix 0 % #item % #place) PlaceRight & set (#hudOptions % #titles) (maybeToList $ fmap (\t -> (Priority 5 (defaultTitleOptions t & set (#style % #size) 0.06 & set #anchoring (-0.5) & set (#style % #color) (view #titleColour s)))) (view #title s))

simpleRectChart ::
  [Double] ->
  Style ->
  ChartOptions
simpleRectChart xs s =
  mempty
    & set #chartTree
      (named "simpleRectChart" [RectChart s rects])
    & set (#hudOptions % #axes)
     [Priority 5 (defaultYAxisOptions & #ticks % #lineTick .~ Nothing & #ticks % #tick .~ TickRound (FormatN FSCommaPrec (Just 2) 4 True True) 5 NoTickExtend)]
  where
    rects = zipWith (\x i -> Rect i (i+1) (min 0 x) (max 0 x)) xs [0..]

-- | histogram chart
histChart ::
  Range Double ->
  Int ->
  [Double] ->
  ChartOptions
histChart r g xs =
  mempty
    & #chartTree
    .~ named "histogram" [RectChart defaultRectStyle rects]
    & #hudOptions
    % #axes
    .~ [Priority 5 (defaultXAxisOptions & #ticks % #lineTick .~ Nothing & #ticks % #tick .~ TickRound (FormatN FSCommaPrec (Just 2) 4 True True) 5 NoTickExtend)]
    & #hudOptions
    % #frames
    .~ [Priority 20 (defaultFrameOptions & #buffer .~ 0.05)]
  where
    hcuts = gridSensible OuterPos False r g
    h = fill hcuts xs
    rects =
      filter (\(Rect _ _ _ y') -> y' /= 0) $
        makeRects (IncludeOvers (NumHask.Space.width r / fromIntegral g)) h

-- | horizontal histogram chart
hhistChart ::
  Range Double ->
  Int ->
  [Double] ->
  ChartOptions
hhistChart r g xs =
  mempty
    & set #chartTree (named "hhistogram" [RectChart defaultRectStyle (flipAxes <$> rects)])
  where
    hcuts = gridSensible OuterPos False r g
    h = fill hcuts xs
    rects = makeRects (IncludeOvers (NumHask.Space.width r / fromIntegral g)) h

-- | horizontal histogram chart
hhistCharts ::
  Range Double ->
  Int ->
  [(Style, [Double])] ->
  ChartOptions
hhistCharts r g xs =
  mempty
    & set #chartTree (named "hhistogram" (zipWith (\r s -> RectChart s  (flipAxes <$> r)) rects (fst <$> xs)))
  where
    hcuts = gridSensible OuterPos False r g
    hs = fill hcuts . snd <$> xs
    rects = makeRects (IncludeOvers (NumHask.Space.width r / fromIntegral g)) <$> hs

-- | scatter chart
scatterChart ::
  [[Point Double]] ->
  [Chart]
scatterChart xss = zipWith (\(s, sh) ps -> GlyphChart (s & set #glyphShape sh) ps) (gpaletteStyle 0.04 0.01) xss

-- | GlyphStyle palette
gpaletteStyle :: Double -> Double -> [(Style, GlyphShape)]
gpaletteStyle s bs = zipWith (\c g -> (defaultGlyphStyle & #size .~ s & #color .~ palette c & #glyphShape .~ g & #borderSize .~ bs, g)) [0 ..] (gpalette <$> [0 .. 8])


-- | Format quantile-style numbers
--
-- >>> quantileNames [0.01, 0.5, 0.99]
-- ["1%","50%","99%"]
quantileNames :: (Functor f) => f Double -> f Text
quantileNames qs = percent commaSF (Just 1) <$> qs

-- | @blendMidLineStyle n w@ produces n lines of width w interpolated between two colors.
blendMidLineStyles :: Int -> Double -> (Colour, Colour) -> [Style]
blendMidLineStyles l w (c1, c2) = lo
  where
    m = (fromIntegral l - 1) / 2 :: Double
    cs = (\x -> 1 - abs (fromIntegral x - m) / m) <$> [0 .. (l - 1)]
    bs = (\x -> mix x c1 c2) <$> cs
    lo = (\c -> defaultLineStyle & #size .~ w & #color .~ c) <$> bs

-- | A histogram based on quantile information
quantileHistChart ::
  -- | quantile names
  Maybe [Text] ->
  -- | quantiles
  [Double] ->
  -- | quantile values
  [Double] ->
  ChartOptions
quantileHistChart names qs vs = mempty & #chartTree .~ unnamed [chart'] & #hudOptions .~ hudOptions
  where
    hudOptions =
      defaultHudOptions
        & #axes
        .~ [ Priority 5 $
               maybe
                 ( axis0
                     & #ticks
                     % #tick
                     .~ TickRound (FormatN FSDecimal (Just 3) 4 True True) 6 TickExtend
                 )
                 ( \x ->
                     axis0
                       & #ticks
                       % #tick
                       .~ TickPlaced (zip vs x)
                 )
                 names
           ]
    axis0 = defaultXAxisOptions & #ticks % #lineTick .~ Nothing & set (#ticks % #textTick %? #style % #size) 0.03
    chart' = RectChart defaultRectStyle hr
    hr =
      zipWith
        (\(y, w) (x, z) -> Rect x z 0 ((w - y) / (z - x)))
        (zip qs (drop 1 qs))
        (zip vs (drop 1 vs))


-- | Surface chart of quantile vs quantile counts
digitSurfaceChart ::
  SurfaceStyle ->
  SurfaceLegendOptions ->
  (Text, Text, Text) ->
  [Text] ->
  [(Int, Int)] ->
  ChartTree
digitSurfaceChart pixelStyle _ ts names ps =
  runHudWith one hs0 (unnamed cs1)
  where
    l = length names - 1
    pts = Point l l
    gr :: Rect Double
    gr = fromIntegral <$> Rect 0 l 0 l
    mapCount = foldl' (\m x -> Map.insertWith (+) x 1.0 m) Map.empty ps
    f :: Point Double -> Double
    f (Point x y) = fromMaybe 0 $ Map.lookup (floor (1 + x), floor (1 + y)) mapCount
    (_, hs0) = toHuds (qvqHud ts names) gr
    (cs1, _) =
      surfacef
        f
        (SurfaceOptions pixelStyle pts gr)

qvqHud :: (Text, Text, Text) -> [Text] -> HudOptions
qvqHud ts labels =
  defaultHudOptions
    & #titles
    .~ titles3 5 ts
    & #axes
    .~ ( Priority 3
           <$> [ defaultYAxisOptions
                   & #ticks
                   % #tick
                   .~ TickPlaced (zip [0 ..] labels)
                   & #ticks
                   % #lineTick
                   .~ Nothing
                   & #ticks
                   % #textTick
                   %? #style
                   % #size
                   .~ 0.03
                   & #place
                   .~ PlaceLeft,
                 defaultXAxisOptions
                   & #ticks
                   % #tick
                   .~ TickPlaced (zip [0 ..] labels)
                   & #ticks
                   % #lineTick
                   .~ Nothing
                   & #ticks
                   % #textTick
                   %? #style
                   % #size
                   .~ 0.03
               ]
       )
