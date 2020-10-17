#!/usr/bin/env stack
-- stack runghc --package reanimate

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Chart
import NumHask.Prelude hiding (fold)
import Control.Lens hiding (transform)
import Graphics.SvgTree.Types hiding (Text, Point, toPoint)
import qualified Graphics.SvgTree.Types as Svg
import Reanimate.Animation
import Reanimate hiding (scale)
import qualified Reanimate as Re
import Reanimate.Svg (unboxFit)
import Linear.V2
import Codec.Picture.Types
import Chart.Reanimate

main :: IO ()
main =
  reanimate $
  addStatic (mkBackgroundPixel (toPixelRGBA8 $ Colour 0.9 0.8 0.9 0.2)) $
  mkAnimation 20 (\x -> tree' [expScaleData (10 * (x-0.5)) $ sinChart])

scaleData :: Double -> Chart Double -> Chart Double
scaleData s c = c & #xys %~ fmap (fmap (s*))

expScaleData :: Double -> Chart Double -> Chart Double
expScaleData s c = c & #xys %~ fmap (fmap ((10.0**s)*))

tree' cs =
  Re.scaleXY (5) (-5) $
  simplify $
  unbox $
  fromHudOptionsChart
  (defaultSvgOptions & #svgHeight .~ 100 & #scaleCharts' .~ ScaleCharts)
  (defaultHudOptions & #hudAxes %~ fmap
   (#atick . #tstyle .~
     TickRound (FormatComma (Just 2)) 8 NoTickExtend)) [] cs

t1 x =
  tree (Chart (RectA defaultRectStyle)
         [RectXY $ (x*) <$> one])

sinChart = Chart (GlyphA defaultGlyphStyle) (PointXY <$> gridP sin (Range 0 (2 * pi)) 30)

