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
  ( simpleLineChart,
    xify,
    yify,
    timeXAxis,
    titles3,
    histChart,
    scatterChart,
    gpalette,
    gpaletteStyle,
    quantileChart,
    digitChart,
    blendMidLineStyles,
    quantileNames,
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
import Data.Bifunctor

-- | convert from [a] to [Point a], by adding the index as the x axis
xify :: [Double] -> [Point Double]
xify ys =
  zipWith Point [0 ..] ys

-- | convert from [a] to [Point a], by adding the index as the y axis
yify :: [Double] -> [Point Double]
yify xs =
  zipWith Point xs [0 ..]

-- | interpret a [Double] as a (poly)line with x coordinates of [0..]
--
-- ![simpleLineChart example](other/simpleline.svg)
simpleLineChart :: Double -> Colour -> [Double] -> Chart
simpleLineChart w c xs =
        LineChart
          (defaultLineStyle & #color .~ c & #size .~ w)
          [xify xs]

-- | Create a hud that has time as the x-axis, based on supplied days, and a rounded yaxis.
timeXAxis :: Int -> [UTCTime] -> AxisOptions
timeXAxis nticks ds =
  defaultAxisOptions & #ticks % #style
      .~ TickPlaced
        ( first (*fromIntegral (length ds)) <$> placedTimeLabelContinuous PosInnerOnly Nothing nticks (unsafeSpace1 ds)
        )

-- | common pattern of chart title, x-axis title and y-axis title
titles3 :: Priority -> (Text,Text,Text) -> [(Priority, Title)]
titles3 p (t,x,y) =
    [ (p, defaultTitle t & #style % #size .~ 0.1),
      (p, defaultTitle x & #place .~ PlaceBottom & #style % #size .~ 0.06),
      (p, defaultTitle y & #place .~ PlaceLeft & #style % #size .~ 0.06)
    ]

-- | histogram chart
--
-- ![histChart example](other/hist.svg)
histChart ::
  Range Double ->
  Int ->
  [Double] ->
  ChartOptions
histChart r g xs =
  mempty &
  #charts .~ named "histogram" [RectChart defaultRectStyle rects] &
  #hudOptions % #axes .~ [(5,defaultAxisOptions & #ticks % #ltick .~ Nothing & #ticks % #style .~ TickRound (FormatN FSCommaPrec (Just 2) 4 True True) 5 NoTickExtend)] &
  #hudOptions % #frames .~ [(20, defaultFrameOptions & #buffer .~ 0.05)]
  where
    hcuts = gridSensible OuterPos False r (fromIntegral g)
    h = fill hcuts xs
    rects = filter (\(Rect _ _ _ y') -> y'/=0) $
      makeRects (IncludeOvers (NumHask.Space.width r / fromIntegral g)) h

-- | scatter chart
--
-- ![scatterChart example](other/scatter.svg)
scatterChart ::
  [[Point Double]] ->
  [Chart]
scatterChart xss = zipWith GlyphChart (gpaletteStyle 0.005) xss

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

-- | Chart template for quantiles.
--
-- ![quantileChart example](other/quantile.svg)
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
              .~
                [( 10, defaultLegendOptions
                    & #textStyle % #size .~ 0.1
                    & #vgap .~ 0.05
                    & #innerPad .~ 0.2
                    & #place .~ PlaceRight
                    & #content .~ zip names ((:[]) <$> c))]
          )
    c =
      zipWith
        (\l x -> LineChart l [x])
        ls
        (zipWith Point [0 ..] <$> xs)

quantileNames :: Functor f => f Double -> f Text
quantileNames qs = percent commaSF Nothing <$> qs

-- | /blendMidLineStyle n w/ produces n lines of width w interpolated between two colors.
blendMidLineStyles :: Int -> Double -> (Colour, Colour) -> [LineStyle]
blendMidLineStyles l w (c1, c2) = lo
  where
    m = (fromIntegral l - 1) / 2 :: Double
    cs = (\x -> 1 - abs (fromIntegral x - m) / m) <$> [0 .. (l - 1)]
    bs = (\x -> mix x c1 c2) <$> cs
    lo = (\c -> defaultLineStyle & #size .~ w & #color .~ c) <$> bs

-- | a chart drawing a histogram based on quantile information
--
-- ![quantileHistChart example](other/qhist.svg)
quantileHistChart ::
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
        .~ [ (5, maybe
               ( axis0 & #ticks % #style
                   .~ TickRound (FormatN FSDecimal (Just 3) 4 True True) 6 TickExtend
               )
               ( \x ->
                   axis0 & #ticks % #style
                     .~ TickPlaced (zip vs x)
               )
               names
           )]
    axis0 = defaultAxisOptions & #ticks % #ltick .~ Nothing & (#ticks % #ttick) %~ fmap (first (#size .~ 0.03))
    chart' = RectChart defaultRectStyle hr
    hr =
      zipWith
        (\(y, w) (x, z) -> Rect x z 0 ((w - y) / (z - x)))
        (zip qs (drop 1 qs))
        (zip vs (drop 1 vs))

-- | a chart drawing deciles of a time series
--
-- ![digitChart example](other/digit.svg)
digitChart ::
  [UTCTime] ->
  [Double] ->
  ChartOptions
digitChart utcs xs =
  mempty & #charts .~ unnamed [c] & #hudOptions .~ hudOptions
  where
    hudOptions =
      defaultHudOptions
        & #axes .~ [(5, timeXAxis 8 utcs)]
    c =
      GlyphChart
        (
            defaultGlyphStyle
                & #color .~ Colour 0 0 1 1
                & #shape .~ CircleGlyph
                & #size .~ 0.01
        )
        (xify xs)

-- | pixel chart of digitized vs digitized counts
--
-- ![digitSurfaceChart example](other/digitsurface.svg)
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

qvqHud :: (Text, Text, Text) -> [Text] -> HudOptions
qvqHud ts labels =
  defaultHudOptions
    & #titles .~ titles3 5 ts
    & #axes
      .~ ((3,) <$> [ defaultAxisOptions
             & #ticks % #style .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #ticks % #ltick .~ Nothing
             & #ticks % #ttick %~ fmap (first (#size .~ 0.03))
             & #place .~ PlaceLeft,
           defaultAxisOptions
             & #ticks % #style .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #ticks % #ltick .~ Nothing
             & #ticks % #ttick %~ fmap (first (#size .~ 0.03))
             & #place .~ PlaceBottom
         ])

-- | Chart for double list of Text.
tableChart :: [[Text]] -> [Chart]
tableChart tss = zipWith (\ts x -> TextChart defaultTextStyle (zip ts (Point x <$> take (length ts) [0 ..]))) tss [0 ..]
