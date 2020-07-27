{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Chart.Various
  ( -- * chart-svg or chart-various transfers
    defaultRender,
    sp,
    xify,
    xify',
    yify,
    yify',
    addLineX,
    addLineY,
    stdLineChart,
    stdLines,
    lineLegend,
    tsAxes,
    titlesHud,
    gpalette,
    gpaletteStyle,
    blendMidLineStyles,

    -- * generic charts
    quantileChart,
    digitChart,
    scatterChart,
    histChart,
    quantileHistChart,
    digitPixelChart,
    tableChart,

    -- * scanner charts
    scanChart,
    scanHud,
    foldScanChart,
    scannerChart,
    scannersChart,
    tsRatesHud,

    -- * chartAny
    -- chartAny,
    write',
    tryChart,
  ) where

import Chart
import NumHask.Prelude hiding (fold)
import Control.Lens
import qualified Data.List as List
import Data.Time (UTCTime(..))
import Data.List ((!!))
import Data.Mealy
import NumHask.Space
import qualified Data.HashMap.Strict as HashMap

defaultRender :: SvgOptions -> (HudOptions, [Chart Double]) -> Text
defaultRender svgo (h, c) = renderHudOptionsChart svgo h [] c

taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

type Rate = Double

sp :: a -> a -> Spot a
sp x y = SpotPoint (Point x y)

-- | convert from [a] to [Point a], by adding the index as the x axis
xify :: [Double] -> [Point Double]
xify ys =
  zipWith Point [0 ..] ys

-- | convert from [a] to [SpotPoint a], by adding the index as the x axis
xify' :: [Double] -> [Spot Double]
xify' ys =
  zipWith sp [0 ..] ys

-- | convert from [a] to [Point a], by adding the index as the y axis
yify :: [Double] -> [Point Double]
yify xs =
  zipWith Point xs [0 ..]

-- | convert from [a] to [SpotPoint a], by adding the index as the y axis
yify' :: [Double] -> [Spot Double]
yify' xs =
  zipWith sp xs [0 ..]

-- | add a horizontal line at y
addLineX :: Double -> LineStyle -> [Chart Double] -> [Chart Double]
addLineX y ls cs = cs <> [l]
  where
    l = Chart (LineA ls) (SpotPoint <$> [Point lx y, Point ux y])
    (Rect lx ux _ _) = fromMaybe unitRect $ foldRect $ mconcat $ fmap toRect . spots <$> cs

-- | add a verticle line at x
addLineY :: Double -> LineStyle -> [Chart Double] -> [Chart Double]
addLineY x ls cs = cs <> [zeroLine]
  where
    zeroLine = Chart (LineA ls) (SpotPoint <$> [Point x ly, Point x uy])
    (Rect _ _ ly uy) = fromMaybe unitRect $ foldRect $ mconcat $ fmap toRect . spots <$> cs

-- | interpret a [[Double]] as a series of lines with x coordinates of [0..]
stdLineChart :: Double -> [Colour] -> [[Double]] -> [Chart Double]
stdLineChart w p xss =
  zipWith
  (\c xs -> Chart (LineA (defaultLineStyle & #color .~ c & #width .~ w))
    ((SpotPoint <$> xify xs)))
  p
  xss

-- | Can of the main palette
stdLines :: Double -> [LineStyle]
stdLines w = (\c -> defaultLineStyle & #color .~ c & #width .~ w) <$> palette1

lineLegend :: Double -> [Text] -> [Colour] -> (LegendOptions, [(Annotation, Text)])
lineLegend w rs cs =
      ( defaultLegendOptions
        & #ltext . #size .~ 0.3
        & #lplace .~ PlaceBottom
        & #legendFrame .~ Just (RectStyle 0.02 (palette1 !! 5) white),
        zipWith
        (\a r -> (LineA a, r))
        ((\c -> defaultLineStyle & #color .~ c & #width .~ w) <$> cs)
        rs
      )

-- Create a hud that has time as the x-axis, based on supplied days, and a rounded yaxis.
tsAxes :: [UTCTime] -> [AxisOptions]
tsAxes ds =
  [ defaultAxisOptions
    & #atick . #tstyle .~ TickRound (FormatPrec (Just 3)) 6 TickExtend
    & #place .~ PlaceLeft,
    defaultAxisOptions & #atick . #tstyle
    .~ TickPlaced
    ( first fromIntegral
      <$> makeTickDates PosIncludeBoundaries Nothing 8 ds
    )
  ]

-- | common pattern of chart title, x-axis title and y-axis title
titlesHud :: Text -> Text -> Text -> HudOptions
titlesHud t x y =
  defaultHudOptions
    & #hudTitles
    .~ [ defaultTitle t,
         defaultTitle x & #place .~ PlaceBottom & #style . #size .~ 0.08,
         defaultTitle y & #place .~ PlaceLeft & #style . #size .~ 0.08
       ]

gpaletteStyle :: Double -> [GlyphStyle]
gpaletteStyle s = zipWith (\c g -> defaultGlyphStyle & #size .~ s & #color .~ c & #shape .~ fst g & #borderSize .~ snd g) palette1 gpalette

gpalette :: [(GlyphShape, Double)]
gpalette =
  [ (CircleGlyph, 0.01 :: Double),
    (SquareGlyph, 0.01),
    (RectSharpGlyph 0.75, 0.01),
    (RectRoundedGlyph 0.75 0.01 0.01, 0.01),
    (EllipseGlyph 0.75, 0),
    (VLineGlyph 0.005, 0.01),
    (HLineGlyph 0.005, 0.01),
    (TriangleGlyph (Point 0.0 0.0) (Point 1 1) (Point 1 0), 0.01),
    (PathGlyph "M0.05,-0.03660254037844387 A0.1 0.1 0.0 0 1 0.0,0.05 0.1 0.1 0.0 0 1 -0.05,-0.03660254037844387 0.1 0.1 0.0 0 1 0.05,-0.03660254037844387 Z", 0.01)
  ]

-- * charts
quantileChart ::
  Text ->
  [Text] ->
  [LineStyle] ->
  [AxisOptions] -> 
  [[Double]] ->
  (HudOptions, [Chart Double])
quantileChart title names ls as xs =
  (hudOptions, chart')
  where
    hudOptions =
      defaultHudOptions
        & #hudTitles .~ [defaultTitle title]
        & ( #hudLegend
              .~ Just
                ( defaultLegendOptions
                    & #ltext . #size .~ 0.1
                    & #vgap .~ 0.05
                    & #innerPad .~ 0.2
                    & #lplace .~ PlaceRight,
                  legendFromChart names chart'
                )
          )
        & #hudAxes .~ as
    chart' =
      zipWith (\l c -> Chart (LineA l) c) ls
      (zipWith (\x y -> SpotPoint (Point x y)) [0 ..] <$> xs)

blendMidLineStyles :: Int -> Double -> (Colour, Colour) -> [LineStyle]
blendMidLineStyles l w (c1, c2) = lo
  where
    m = (fromIntegral l - 1) / 2 :: Double
    cs = (\x -> 1 - abs (fromIntegral x - m) / m) <$> [0 .. (l - 1)]
    bs = (\x -> blend x c1 c2) <$> cs
    lo = (\c -> defaultLineStyle & #width .~ w & #color .~ c) <$> bs

-- | a chart showing a time series of digitized values (eg this return was a 30th percentile)
digitChart ::
  Text ->
  [UTCTime] ->
  [Double] ->
  (HudOptions, [Chart Double])
digitChart title utcs xs =
  (hudOptions, [c])
  where
    hudOptions =
      defaultHudOptions
        & #hudTitles .~ [defaultTitle title]
        & #hudAxes .~ tsAxes utcs
    c = Chart (GlyphA (defaultGlyphStyle &
                        #color .~ Colour 0 0 1 1 &
                        #shape .~ CircleGlyph & #size .~ 0.01))
             (SpotPoint <$> xify xs)

-- | scatter chart
scatterChart ::
  [[Point Double]] ->
  [Chart Double]
scatterChart xss = zipWith (\gs xs -> Chart (GlyphA gs) (SpotPoint <$> xs)) (gpaletteStyle 0.02) xss

-- | histogram chart
histChart ::
  Text ->
  Maybe [Text] ->
  Range Double ->
  Int ->
  [Double] ->
  (HudOptions, [Chart Double])
histChart title names r g xs =
  barChart defaultBarOptions barData
  & first (#hudTitles .~ [defaultTitle title])
  where
    barData = BarData [hr] names Nothing
    hcuts = grid OuterPos r g
    h = fill hcuts xs
    hr = (\(Rect x x' _ _) -> (x+x')/2) <$>
      makeRects (IncludeOvers (NumHask.Space.width r / fromIntegral g)) h

-- | a chart drawing a histogram based on quantile information
quantileHistChart ::
  Text ->
  Maybe [Text] ->
  -- | quantiles
  [Double] ->
  -- | quantile values
  [Double] ->
  (HudOptions, [Chart Double])
quantileHistChart title names qs vs = (hudOptions, [chart'])
  where
    hudOptions =
      defaultHudOptions
        & #hudTitles
        .~ [defaultTitle title]
        & #hudAxes
        .~ [ maybe
               (defaultAxisOptions & #atick . #tstyle .~
                TickRound (FormatPrec (Just 3)) 8 TickExtend)
               ( \x ->
                   defaultAxisOptions & #atick . #tstyle
                     .~ TickPlaced (zip vs x)
               )
               names
           ]
    chart' = Chart (RectA defaultRectStyle) (SpotRect <$> hr)
    hr =
      zipWith (\(y, w) (x, z) -> Rect x z 0 ((w - y) / (z - x)))
      (zip qs (drop 1 qs))
      (zip vs (drop 1 vs))

-- | pixel chart of digitized vs digitized counts
digitPixelChart ::
  PixelStyle ->
  PixelLegendOptions ->
  (Text, Text, Text) ->
  [Text] ->
  [(Int, Int)] ->
  [Chart Double]
digitPixelChart pixelStyle plo ts names ps =
  runHud (aspect 1) (hs0 <> hs1) (cs0 <> cs1)
  where
    l = length names
    pts = Point l l
    gr :: Rect Double
    gr = fromIntegral <$> Rect 0 l 0 l
    mapCount = foldl' (\m x -> HashMap.insertWith (+) x 1.0 m) HashMap.empty ps
    f :: Point Double -> Double
    f (Point x y) = fromMaybe 0 $ HashMap.lookup (fromIntegral (floor x :: Integer), fromIntegral (floor y :: Integer)) mapCount
    (hs0, cs0) = makeHud gr (qvqHud ts names)
    (cs1, hs1) =
      pixelfl
        f
        (PixelOptions pixelStyle pts gr)
        plo

-- style helpers
qvqHud :: (Text, Text, Text) -> [Text] -> HudOptions
qvqHud ts labels =
  defaultHudOptions
    & #hudTitles .~ makeTitles ts
    & #hudAxes
      .~ [ defaultAxisOptions
             & #atick . #tstyle .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #place .~ PlaceLeft,
           defaultAxisOptions
             & #atick . #tstyle .~ TickPlaced (zip ((0.5 +) <$> [0 ..]) labels)
             & #place .~ PlaceBottom
         ]

makeTitles :: (Text, Text, Text) -> [Title]
makeTitles (t, xt, yt) =
  reverse
    [ defaultTitle t,
      defaultTitle xt & #place .~ PlaceBottom & #style . #size .~ 0.06,
      defaultTitle yt & #place .~ PlaceLeft & #style . #size .~ 0.06
    ]

tableChart :: [[Text]] -> [Chart Double]
tableChart tss = zipWith (\ts x -> Chart (TextA defaultTextStyle ts) (sp x <$> take (length ts) [0..])) tss [0..]

-- * scanners
-- | simple scan of a time series through a Mealy using a list of rates, with time dimension of [0..]
scanChart :: (Rate -> Mealy a Double) -> [Rate] -> Int -> [a] -> [Chart Double]
scanChart m rates d xs =
  zipWith (\s xs' -> Chart (LineA s) xs')
  (stdLines 0.003)
  (zipWith sp (fromIntegral <$> [d..]) <$> ((\r -> drop d $ scan (m r) xs) <$> rates))

-- | common line chart hud with rates as a legend
scanHud :: Double -> Text -> [Double] -> HudOptions
scanHud w t rates = 
  defaultHudOptions &
     #hudTitles .~ [ defaultTitle t] &
     #hudLegend .~ Just (lineLegend w (("rate = " <>) . show <$> rates) palette1)

-- | fold over a scanned time series by rates
foldScanChart :: (Rate -> Mealy a b) -> (Rate -> Mealy b Double) -> [Rate] -> [a] -> [Chart Double]
foldScanChart scan' fold' rates xs =
    (: []) $
        Chart
          (LineA defaultLineStyle)
          (zipWith sp rates ((\r -> fold (fold' r) $ scan (scan' r) xs) <$> rates))

zeroLineStyle :: LineStyle
zeroLineStyle = defaultLineStyle & #color .~ (palette1!!7) & #width .~ 0.002

-- | take a decaying scanner, a list of decay rates, and create linecharts from an [a]
scannerChart :: Int -> [Double] -> (Double -> [a] -> [Double]) -> [a] -> [Chart Double]
scannerChart n rs rscan xs =
  addLineX 0 zeroLineStyle $
  stdLineChart 0.005 palette1 (tsrs n rs rscan xs)
  where
    tsrs n rs rscan xs = taker n . (`rscan` xs) <$> rs

-- | take a multi-decaying scanner, a decay rate, and create linecharts from an [a]
scannersChart :: Int -> Double -> (Double -> [a] -> [[Double]]) -> [a] -> [Chart Double]
scannersChart n r rscan xs =
  addLineX 0 zeroLineStyle $
  stdLineChart 0.005 palette1 (tsrs n r rscan xs)
  where
    tsrs n r rscan xs = taker n <$> (List.transpose $ rscan r xs)

-- | a Hud for time series with a rates legend
tsRatesHud :: Text -> [Double] -> [UTCTime] -> HudOptions
tsRatesHud title rs ds =
  defaultHudOptions
    & #hudTitles
    .~ [defaultTitle title & #style . #size .~ 0.08]
    & #hudLegend
    .~ Just (lineLegend 0.001 ((("rate: " <>) . show) <$> rs) palette1)
    & #hudAxes .~ tsAxes ds

chartAny :: Text -> Either Text Text
chartAny t = maybe (Left "<html>bad read</html>") Right . head . rights $
  [ -- single list
    tryChart t (\xs -> bool
                 (let (h,c) = barChart defaultBarOptions (BarData [xs] Nothing Nothing) in renderHudOptionsChart defaultSvgOptions h [] c)
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
    tryChart ("\"" <> t <> "\"") (\x -> renderHudOptionsChart defaultSvgOptions mempty [] [Chart (TextA defaultTextStyle ["\"" <> x <> "\""]) [sp 0 0]])
  ]

chartList2 :: [[Double]] -> Text
chartList2 xss
  | (length xss < 4) && (length (xss!!0) < 10) = anyBarChart xss
    -- square
  | all (length xss ==) (length <$> xss) =
    anyPixelChart xss
  | otherwise = anyLineChart xss

chartText1 :: [Text] -> Text
chartText1 xs
  | (length xs < 20) = anyTextChart [xs]
    -- word salad
  | otherwise = anySingleNamedBarChart (second fromIntegral <$> HashMap.toList mapCount)
  where
    mapCount = foldl' (\m x -> HashMap.insertWith (+) x (1::Int) m) HashMap.empty xs

anyBarChart :: [[Double]] -> Text
anyBarChart xss = renderHudOptionsChart defaultSvgOptions h [] c
  where
    (h,c) = barChart defaultBarOptions
      (BarData xss
       (Just (("row " <>) . show <$> take nx [(0::Int)..]))
       (Just (("col " <>) . show <$> take ny [(0::Int)..]))
      )
      where
        nx = length xss
        ny = maximum (length <$> xss)

anySingleNamedBarChart :: [(Text, Double)] -> Text
anySingleNamedBarChart xs = renderHudOptionsChart defaultSvgOptions h [] c
  where
    (h,c) = barChart defaultBarOptions
      (BarData [snd <$> xs]
       (Just (fst <$> xs))
       Nothing
      )

anyLineChart :: [[Double]] -> Text
anyLineChart xss =
  renderHudOptionsChart defaultSvgOptions defaultHudOptions [] (stdLineChart 0.02 palette1 xss)

anyScatterChart :: [[(Double, Double)]] -> Text
anyScatterChart xss =
  renderHudOptionsChart defaultSvgOptions defaultHudOptions [] (scatterChart (fmap (fmap (uncurry Point)) xss))

anyTextChart :: [[Text]] -> Text
anyTextChart xss =
  renderHudOptionsChart defaultSvgOptions defaultHudOptions [] (tableChart xss)

anyPixelChart :: [[Double]] -> Text
anyPixelChart xss = renderHudOptionsChart defaultSvgOptions (anyPixelHud nx ny) h c
  where
    (c,h) =
      pixelfl
      (\(Point x y) -> ((xss !! (floor x)) !! (floor y)))
      (PixelOptions defaultPixelStyle (Point nx ny) (Rect 0 (fromIntegral nx :: Double) 0 (fromIntegral ny)))
      (defaultPixelLegendOptions "square")
    nx = length xss
    ny = length (xss!!0)

anyPixelHud :: Int -> Int -> HudOptions
anyPixelHud nx ny =
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
    labelsx = show <$> [0..(nx - 1)]
    labelsy = show <$> [0..(ny - 1)]

tryChart :: (Read a) => Text -> (a -> Text) -> Either Text Text
tryChart t c = either (Left . pack) (Right . c) $ readEither (unpack t)

write' :: Text -> IO ()
write' t = writeFile "scratch.svg" $ either id id $ chartAny t
