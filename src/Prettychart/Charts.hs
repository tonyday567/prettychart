{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Various common chart patterns.
module Prettychart.Charts
  ( simpleLineChart,
    xify,
    yify,
    timeXAxis,
    titles3,
    histChart,
    gpalette,
    scatterChart,
    blendMidLineStyles,
    quantileNames,
    quantileChart,
    digitChart,
    quantileHistChart,
    digitSurfaceChart,
  )
where

import Chart hiding (abs)
import Data.Bifunctor
import Data.Foldable
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Time (UTCTime (..))
import NumHask.Space
import Optics.Core

-- $setup
--
-- >>> :set -Wno-type-defaults
-- >>> import Chart
-- >>> import Prettychart.Charts
-- >>> import Data.Text (pack, Text)
-- >>> import qualified Data.Text as Text
-- >>> import qualified Data.Text.IO as Text

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
--
simpleLineChart :: Double -> Colour -> [Double] -> Chart
simpleLineChart w c xs =
  LineChart
    (defaultLineStyle & #color .~ c & #size .~ w)
    [xify xs]

-- | Create a hud that has time as the x-axis, based on supplied UTCTime list.
timeXAxis :: Int -> [UTCTime] -> AxisOptions
timeXAxis nticks ds =
  defaultAxisOptions
    & #ticks % #style
      .~ TickPlaced
        ( first (* fromIntegral (length ds)) <$> placedTimeLabelContinuous PosInnerOnly Nothing nticks (unsafeSpace1 ds)
        )

-- | common pattern of chart title, x-axis title and y-axis title
titles3 :: Priority -> (Text, Text, Text) -> [(Priority, Title)]
titles3 p (t, x, y) =
  [ (p, defaultTitle t & #style % #size .~ 0.08),
    (p, defaultTitle x & #place .~ PlaceBottom & #style % #size .~ 0.05),
    (p, defaultTitle y & #place .~ PlaceLeft & #style % #size .~ 0.05)
  ]

-- | histogram chart
--
histChart ::
  Range Double ->
  Int ->
  [Double] ->
  ChartOptions
histChart r g xs =
  mempty
    & #charts .~ named "histogram" [RectChart defaultRectStyle rects]
    & #hudOptions % #axes .~ [(5, defaultAxisOptions & #ticks % #ltick .~ Nothing & #ticks % #style .~ TickRound (FormatN FSCommaPrec (Just 2) 4 True True) 5 NoTickExtend)]
    & #hudOptions % #frames .~ [(20, defaultFrameOptions & #buffer .~ 0.05)]
  where
    hcuts = gridSensible OuterPos False r g
    h = fill hcuts xs
    rects =
      filter (\(Rect _ _ _ y') -> y' /= 0) $
        makeRects (IncludeOvers (NumHask.Space.width r / fromIntegral g)) h

-- | scatter chart
--
scatterChart ::
  [[Point Double]] ->
  [Chart]
scatterChart xss = zipWith GlyphChart (gpaletteStyle 0.04 0.01) xss

-- | GlyphStyle palette
gpaletteStyle :: Double -> Double -> [GlyphStyle]
gpaletteStyle s bs = zipWith (\c g -> defaultGlyphStyle & #size .~ s & #color .~ palette1 c & #shape .~ g & #borderSize .~ bs) [0 ..] gpalette

-- | Glyph palette
gpalette :: [GlyphShape]
gpalette =
  [ CircleGlyph,
    SquareGlyph,
    RectSharpGlyph 0.75,
    RectRoundedGlyph 0.75 0.01 0.01,
    EllipseGlyph 0.75,
    VLineGlyph,
    HLineGlyph,
    TriangleGlyph (Point 0.0 0.0) (Point 1 1) (Point 1 0),
    PathGlyph "M0.05,-0.03660254037844387 A0.1 0.1 0.0 0 1 0.0,0.05 0.1 0.1 0.0 0 1 -0.05,-0.03660254037844387 0.1 0.1 0.0 0 1 0.05,-0.03660254037844387 Z" ScaleBorder
  ]

-- | Chart template for quantiles.
--
quantileChart ::
  [Text] ->
  [LineStyle] ->
  [[Double]] ->
  ChartOptions
quantileChart names ls xs = mempty & #hudOptions .~ h & #charts .~ unnamed c
  where
    h =
      defaultHudOptions
        & ( #legends
              .~ [ ( 10,
                     defaultLegendOptions
                       & #textStyle % #size .~ 0.1
                       & #vgap .~ 0.05
                       & #innerPad .~ 0.2
                       & #place .~ PlaceRight
                       & #legendCharts .~ zip names ((: []) <$> c)
                   )
                 ]
          )
    c =
      zipWith
        (\l x -> LineChart l [x])
        ls
        (zipWith Point [0 ..] <$> xs)

-- | Format quantile-style numbers
--
-- >>> quantileNames [0.01, 0.5, 0.99]
-- ["1%","50%","99%"]
quantileNames :: (Functor f) => f Double -> f Text
quantileNames qs = percent commaSF Nothing <$> qs

-- | @blendMidLineStyle n w@ produces n lines of width w interpolated between two colors.
blendMidLineStyles :: Int -> Double -> (Colour, Colour) -> [LineStyle]
blendMidLineStyles l w (c1, c2) = lo
  where
    m = (fromIntegral l - 1) / 2 :: Double
    cs = (\x -> 1 - abs (fromIntegral x - m) / m) <$> [0 .. (l - 1)]
    bs = (\x -> mix x c1 c2) <$> cs
    lo = (\c -> defaultLineStyle & #size .~ w & #color .~ c) <$> bs

-- | A histogram based on quantile information
--
quantileHistChart ::
  -- | quantile names
  Maybe [Text] ->
  -- | quantiles
  [Double] ->
  -- | quantile values
  [Double] ->
  ChartOptions
quantileHistChart names qs vs = mempty & #charts .~ unnamed [chart'] & #hudOptions .~ hudOptions
  where
    hudOptions =
      defaultHudOptions
        & #axes
          .~ [ ( 5,
                 maybe
                   ( axis0
                       & #ticks % #style
                         .~ TickRound (FormatN FSDecimal (Just 3) 4 True True) 6 TickExtend
                   )
                   ( \x ->
                       axis0
                         & #ticks % #style
                           .~ TickPlaced (zip vs x)
                   )
                   names
               )
             ]
    axis0 = defaultAxisOptions & #ticks % #ltick .~ Nothing & (#ticks % #ttick) %~ fmap (first (#size .~ 0.03))
    chart' = RectChart defaultRectStyle hr
    hr =
      zipWith
        (\(y, w) (x, z) -> Rect x z 0 ((w - y) / (z - x)))
        (zip qs (drop 1 qs))
        (zip vs (drop 1 vs))

-- | A chart drawing quantiles of a time series
--
digitChart ::
  [UTCTime] ->
  [Double] ->
  [Text] ->
  ChartOptions
digitChart utcs xs labels =
  mempty & #charts .~ unnamed [c] & #hudOptions .~ hudOptions
  where
    hudOptions =
      defaultHudOptions
        & #axes .~ [(5, timeXAxis 8 utcs), (5, decileYAxis labels)]
    c =
      GlyphChart
        ( defaultGlyphStyle
            & #color .~ Colour 0 0 1 1
            & #shape .~ CircleGlyph
            & #size .~ 0.01
        )
        (xify xs)

decileYAxis :: [Text] -> AxisOptions
decileYAxis labels =
  defaultAxisOptions
    & #ticks % #style .~ TickPlaced (zip ((+ 0.5) <$> [0 ..]) labels)
    & #ticks % #ltick .~ Nothing
    & #ticks % #ttick %~ fmap (first (#size .~ 0.03))
    & #place .~ PlaceLeft

-- | Surface chart of quantile vs quantile counts
--
digitSurfaceChart ::
  SurfaceStyle ->
  SurfaceLegendOptions ->
  (Text, Text, Text) ->
  [Text] ->
  [(Int, Int)] ->
  ChartTree
digitSurfaceChart pixelStyle _ ts names ps =
  runHud (aspect 1) hs0 (unnamed cs1)
  where
    l = length names - 1
    pts = Point l l
    gr :: Rect Double
    gr = fromIntegral <$> Rect 0 l 0 l
    mapCount = foldl' (\m x -> Map.insertWith (+) x 1.0 m) Map.empty ps
    f :: Point Double -> Double
    f (Point x y) = fromMaybe 0 $ Map.lookup (floor (1 + x), floor (1 + y)) mapCount
    (hs0, _) = toHuds (qvqHud ts names) gr
    (cs1, _) =
      surfacef
        f
        (SurfaceOptions pixelStyle pts gr)

qvqHud :: (Text, Text, Text) -> [Text] -> HudOptions
qvqHud ts labels =
  defaultHudOptions
    & #titles .~ titles3 5 ts
    & #axes
      .~ ( (3,)
             <$> [ defaultAxisOptions
                     & #ticks % #style .~ TickPlaced (zip [0 ..] labels)
                     & #ticks % #ltick .~ Nothing
                     & #ticks % #ttick %~ fmap (first (#size .~ 0.03))
                     & #place .~ PlaceLeft,
                   defaultAxisOptions
                     & #ticks % #style .~ TickPlaced (zip [0 ..] labels)
                     & #ticks % #ltick .~ Nothing
                     & #ticks % #ttick %~ fmap (first (#size .~ 0.03))
                     & #place .~ PlaceBottom
                 ]
         )
