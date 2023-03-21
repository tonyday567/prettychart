{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use ?~" #-}
{-# LANGUAGE TupleSections #-}

-- | Various common chart patterns.
module Chart.Various
  ( -- * sub-chart patterns
    xify,
    yify,
    addLineX,
    addLineY,
    simpleLineChart,
    timeXAxis,
    titlesHud,
    gpalette,
    gpaletteStyle,
    blendMidLineStyles,

    -- * chart patterns
    quantileChart,
    digitChart,
    scatterChart,
    histChart,
    quantileHistChart,

    digitSurfaceChart,
    tableChart,
  )
where

import Chart
import Data.Time (UTCTime (..))
import NumHask.Prelude hiding (fold)
import NumHask.Space
import Data.Text (Text)
import Optics.Core
import qualified Data.Map.Strict as Map


-- | convert from [a] to [Point a], by adding the index as the x axis
xify :: [Double] -> [Point Double]
xify ys =
  zipWith Point [0 ..] ys

-- | convert from [a] to [Point a], by adding the index as the y axis
yify :: [Double] -> [Point Double]
yify xs =
  zipWith Point xs [0 ..]

-- | add a horizontal line at y
addLineX :: Double -> LineStyle -> [Chart] -> [Chart]
addLineX y ls cs = cs <> [l]
  where
    l = LineChart ls [[Point lx y, Point ux y]]
    (Rect lx ux _ _) = fromMaybe one $ boxes cs

-- | add a verticle line at x
addLineY :: Double -> LineStyle -> [Chart] -> [Chart]
addLineY x ls cs = cs <> [zeroLine]
  where
    zeroLine = LineChart ls [[Point x ly, Point x uy]]
    (Rect _ _ ly uy) = fromMaybe one $ boxes cs

-- | interpret a [Double] as a (poly)line with x coordinates of [0..]
simpleLineChart :: Double -> Colour -> [Double] -> Chart
simpleLineChart w c xs =
        LineChart
          (defaultLineStyle & #color .~ c & #size .~ w)
          [xify xs]

-- | Create a hud that has time as the x-axis, based on supplied days, and a rounded yaxis.
timeXAxis :: [UTCTime] -> AxisOptions
timeXAxis ds =
  defaultAxisOptions & #ticks % #style
      .~ TickPlaced
        ( placedTimeLabelContinuous PosIncludeBoundaries Nothing 8 (unsafeSpace1 ds)
        )

-- | common pattern of chart title, x-axis title and y-axis title
titlesHud :: Text -> Text -> Text -> HudOptions
titlesHud t x y =
  defaultHudOptions
    & #titles
    .~ [ (5, defaultTitle t),
         (5, defaultTitle x & #place .~ PlaceBottom & #style % #size .~ 0.08),
         (5, defaultTitle y & #place .~ PlaceLeft & #style % #size .~ 0.08)
       ]

-- | GlyphStyle palette
gpaletteStyle :: Double -> [GlyphStyle]
gpaletteStyle s = zipWith (\c g -> defaultGlyphStyle & #size .~ s & #color .~ palette1 c & #shape .~ fst g & #borderSize .~ snd g) [0..] gpalette

-- | Glyph palette
gpalette :: [(GlyphShape, Double)]
gpalette =
  [ (CircleGlyph, 0.01 :: Double),
    (SquareGlyph, 0.01),
    (RectSharpGlyph 0.75, 0.01),
    (RectRoundedGlyph 0.75 0.01 0.01, 0.01),
    (EllipseGlyph 0.75, 0),
    (VLineGlyph, 0.01),
    (HLineGlyph, 0.01),
    (TriangleGlyph (Point 0.0 0.0) (Point 1 1) (Point 1 0), 0.01),
    (PathGlyph "M0.05,-0.03660254037844387 A0.1 0.1 0.0 0 1 0.0,0.05 0.1 0.1 0.0 0 1 -0.05,-0.03660254037844387 0.1 0.1 0.0 0 1 0.05,-0.03660254037844387 Z" ScaleBorder, 0.01)
  ]

-- * charts

-- | Chart template for quantiles.
quantileChart ::
  Text ->
  [LineStyle] ->
  [AxisOptions] ->
  [[Double]] ->
  (HudOptions, [Chart])
quantileChart title ls as xs =
  (hudOptions, chart')
  where
    hudOptions =
      defaultHudOptions
        & #titles .~ [(5, defaultTitle title)]
        & ( #legends
              .~
                [( 10, defaultLegendOptions
                    & #textStyle % #size .~ 0.1
                    & #vgap .~ 0.05
                    & #innerPad .~ 0.2
                    & #place .~ PlaceRight )]
          )
        & #axes .~ ((6,) <$> as)

    chart' =
      zipWith
        LineChart
        ls
        [zipWith Point [0 ..] <$> xs]

-- | /blendMidLineStyle n w/ produces n lines of width w interpolated between two colors.
blendMidLineStyles :: Int -> Double -> (Colour, Colour) -> [LineStyle]
blendMidLineStyles l w (c1, c2) = lo
  where
    m = (fromIntegral l - 1) / 2 :: Double
    cs = (\x -> 1 - abs (fromIntegral x - m) / m) <$> [0 .. (l - 1)]
    bs = (\x -> mix x c1 c2) <$> cs
    lo = (\c -> defaultLineStyle & #size .~ w & #color .~ c) <$> bs

digitChart ::
  Text ->
  [UTCTime] ->
  [Double] ->
  (HudOptions, [Chart])
digitChart title utcs xs =
  (hudOptions, [c])
  where
    hudOptions =
      defaultHudOptions
        & #titles .~ [(5, defaultTitle title)]
        & #axes .~ [(5, timeXAxis utcs)]
    c =
      GlyphChart
        (
            defaultGlyphStyle
                & #color .~ Colour 0 0 1 1
                & #shape .~ CircleGlyph
                & #size .~ 0.01
        )
        (xify xs)

-- | scatter chart
scatterChart ::
  [[Point Double]] ->
  [Chart]
scatterChart xss = zipWith GlyphChart (gpaletteStyle 0.02) xss

-- | histogram chart
histChart ::
  Text ->
  [Text] ->
  Range Double ->
  Int ->
  [Double] ->
  ChartOptions
histChart title names r g xs =
  barChart defaultBarOptions barData
    & (#hudOptions % #titles .~ [(5,defaultTitle title)])
  where
    barData = BarData [hr] names []
    hcuts = grid OuterPos r g
    h = fill hcuts xs
    hr =
      (\(Rect x x' _ _) -> (x + x') / 2)
        <$> makeRects (IncludeOvers (NumHask.Space.width r / fromIntegral g)) h

-- | a chart drawing a histogram based on quantile information
quantileHistChart ::
  Text ->
  Maybe [Text] ->
  -- | quantiles
  [Double] ->
  -- | quantile values
  [Double] ->
  (HudOptions, [Chart])
quantileHistChart title names qs vs = (hudOptions, [chart'])
  where
    hudOptions =
      defaultHudOptions
        & #titles
        .~ [(5,defaultTitle title)]
        & #axes
        .~ [ (5, maybe
               ( defaultAxisOptions & #ticks % #style
                   .~ TickRound (FormatN FSDecimal (Just 3) True) 6 TickExtend
               )
               ( \x ->
                   defaultAxisOptions & #ticks % #style
                     .~ TickPlaced (zip vs x)
               )
               names
           )]
    chart' = RectChart defaultRectStyle hr
    hr =
      zipWith
        (\(y, w) (x, z) -> Rect x z 0 ((w - y) / (z - x)))
        (zip qs (drop 1 qs))
        (zip vs (drop 1 vs))

-- | pixel chart of digitized vs digitized counts
digitSurfaceChart ::
  SurfaceStyle ->
  SurfaceLegendOptions ->
  (Text, Text, Text) ->
  [Text] ->
  [(Int, Int)] ->
  ChartTree
digitSurfaceChart pixelStyle plo ts names ps =
  runHud (aspect 1) (hs0 <> hs1) (unnamed cs1)
  where
    l = length names
    pts = Point l l
    gr :: Rect Double
    gr = fromIntegral <$> Rect 0 l 0 l
    mapCount = foldl' (\m x -> Map.insertWith (+) x 1.0 m) Map.empty ps
    f :: Point Double -> Double
    f (Point x y) = fromMaybe 0 $ Map.lookup (floor x, floor y) mapCount
    (hs0, _) = toHuds (qvqHud ts names) gr
    (cs1, hs1) =
      surfacefl
        f
        (SurfaceOptions pixelStyle pts gr)
        plo

-- style helpers
qvqHud :: (Text, Text, Text) -> [Text] -> HudOptions
qvqHud ts labels =
  defaultHudOptions
    & #titles .~ ((5,) <$> makeTitles ts)
    & #axes
      .~ ((5,) <$> [ defaultAxisOptions
             & #ticks % #style .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #place .~ PlaceLeft,
           defaultAxisOptions
             & #ticks % #style .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #place .~ PlaceBottom
         ])

makeTitles :: (Text, Text, Text) -> [Title]
makeTitles (t, xt, yt) =
  reverse
    [ defaultTitle t,
      defaultTitle xt & #place .~ PlaceBottom & #style % #size .~ 0.06,
      defaultTitle yt & #place .~ PlaceLeft & #style % #size .~ 0.06
    ]

-- | Chart for double list of Text.
tableChart :: [[Text]] -> [Chart]
tableChart tss = zipWith (\ts x -> TextChart defaultTextStyle (zip ts (Point x <$> take (length ts) [0 ..]))) tss [0 ..]
