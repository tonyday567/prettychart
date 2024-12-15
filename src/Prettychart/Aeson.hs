{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Prettychart.Aeson where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import NumHask.Space
import Chart
import Optics.Core
import Data.Bool
import Data.Maybe

data GD = GD {isosamples:: [(Double,Double,Double)], gradd :: [[Double]], mom :: [[Double]], adam :: [[Double]]} deriving (Generic)

instance FromJSON GD where
  parseJSON = parseGD

parseGD :: Value -> Parser GD
parseGD = withObject "gradient descent" $ \obj -> do
  isosamples <- obj .: "isosamples"
  gd <- obj .: "gradient-descent"
  momentum <- obj .: "momentum"
  adam <- obj .: "adam"
  pure (GD isosamples gd momentum adam)

data ConfigChartGD = ConfigChartGD { scolors :: [Int], dotsize :: Double, gap :: Double, includeSurface :: Bool, bigFrame :: Maybe Double, surfaceSloRect :: Rect Double, surfaceOpac :: Double, surfaceLight :: Double, sloTextSize :: Double, placeHos :: Place, dotBorderSize :: Double, dotBorderSizeLegend :: Double, legendSize :: Double } deriving (Generic)

defaultConfigChartGD :: ConfigChartGD
defaultConfigChartGD = ConfigChartGD [3..7] 0.016 (8 / 99.0) True (Just 0.1) (Rect 0.35 0.45 0.1 0.4) 0.7 0.45 0.03 (PlaceAbsolute (Point 0.65 (-0.15))) 0.005 0.04 0.25

chartGD :: ConfigChartGD -> GD -> ChartOptions
chartGD cfg gd = l
  where
    xs = isosamples gd
    rx' = unsafeSpace1 @(Range Double) ((\(x,_,_) -> x) <$> xs)
    ry' = unsafeSpace1 @(Range Double) ((\(_,x,_) -> x) <$> xs)
    dr = unsafeSpace1 @(Range Double) ((\(_,_,x) -> x) <$> xs)
    r = Ranges rx' ry'
    evenColors = set opac' (view #surfaceOpac cfg) . trimColour . over lightness' (const (view #surfaceLight cfg)) . palette <$> view #scolors cfg
    mksd gap (x,y,z) = SurfaceData (Rect x (x+gap) y (y+gap)) (flip mixes evenColors . project dr (Range 0 1) $ z)
    sds = mksd (view #gap cfg) <$> xs

    slo = defaultSurfaceLegendOptions & set (#sloSurfaceStyle % #surfaceColors) evenColors & set #sloDataRange dr & set #sloRect (view #surfaceSloRect cfg) & set #sloWidth 0.1 & set (#sloAxisOptions % #ticks % #textTick % _Just % #style % #size) (view #sloTextSize cfg)

    ss = zipWith (\c g -> defaultGlyphStyle & set #glyphShape g & set #size (view #dotsize cfg) & set #borderSize (view #dotBorderSize cfg) & set #color (palette c) & set #borderColor (set lightness' 0.5 (palette c))) [0..] [CircleGlyph, SquareGlyph, EllipseGlyph 0.6]

    sc = named "surface" (surfaces clear sds)
    gdc = named "gd" [GlyphChart (ss!!0) (fmap (\[x,y] -> Point x y) (gradd gd))]
    ac = named "adam" [GlyphChart (ss!!1) (fmap (\[x,y] -> Point x y) (adam gd))]
    mc = named "mom" [GlyphChart (ss!!2) (fmap (\[x,y] -> Point x y) (mom gd))]

    getc1 cs = cs & toListOf charts' & mconcat & take 1

    hos = defaultHudOptions & set (#axes % each % #item % #ticks % #lineTick) Nothing & set (#axes % each % #item % #ticks % #tick % tickExtend') (Just NoTickExtend) & set #legends [ Priority 12 $ defaultLegendOptions & set #place (view #placeHos cfg) & set #frame Nothing & set #legendCharts [("gd", getc1 gdc), ("mom", getc1 mc),("adam", getc1 ac)] & set #legendSize (view #legendSize cfg) & set #scaleChartsBy (view #legendSize cfg) & set (#legendCharts % each % _2 % each % #chartStyle % #borderSize) (view #dotBorderSizeLegend cfg)]

    bc = unnamed [BlankChart clear [r]]
    sc' = addHud ChartAspect hos (bool bc sc (view #includeSurface cfg) <> gdc <> ac <> mc)

    grc = gridReferenceChart slo
    hoLegend = (mempty :: HudOptions) & set #axes [Priority 1 (view #sloAxisOptions slo & set (#ticks % #textTick % _Just % #buffer) 0.001 & set (#ticks % #glyphTick % _Just % #buffer) 0)]
    grcLegend = addHud (FixedAspect (view #sloWidth slo)) hoLegend grc
    ctbox = fromMaybe one (view styleBox' sc')
    legbox = projectOnR ctbox one (view #sloRect slo)
    lsc = projectChartTree legbox grcLegend

    l = mempty @ChartOptions & set #chartTree (lsc <> sc') & over (#hudOptions % #frames) (<> foldMap (\x -> [Priority 100 (defaultFrameOptions & set #buffer x)]) (view #bigFrame cfg)) & set (#markupOptions % #cssOptions % #shapeRendering) UseCssCrisp
