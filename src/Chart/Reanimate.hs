{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{- | Read some text, and attempt to make default charts.

-}
module Chart.Reanimate where

import Chart
import NumHask.Prelude hiding (fold)
import Control.Lens hiding (transform)
import Graphics.SvgTree.Types hiding (Text, Point)
import qualified Graphics.SvgTree.Types as Svg
-- import Reanimate.Animation
-- import Reanimate hiding (scale)
import Linear.V2
import Codec.Picture.Types
import Graphics.SvgTree.PathParser
import qualified Data.Attoparsec.Text as A

-- * color conversions
toPixelRGBA8 :: Colour -> PixelRGBA8
toPixelRGBA8 (Colour r g b o) =
  PixelRGBA8
  (fromIntegral $ (floor $ r * 256 :: Int))
  (fromIntegral $ (floor $ g * 256 :: Int))
  (fromIntegral $ (floor $ b * 256 :: Int))
  (fromIntegral $ (floor $ o * 256 :: Int))

-- | Render a chart using the supplied svg and hud config.
fromHudOptionsChart :: SvgOptions -> HudOptions -> [Hud Double] -> [Chart Double] -> Document
fromHudOptionsChart so hc hs cs = fromHudChart so (hs <> hs') (cs <> cs')
  where
    (hs', cs') = makeHud (fixRect $ dataBox cs) hc

-- | Render some huds and charts.
fromHudChart :: SvgOptions -> [Hud Double] -> [Chart Double] -> Document
fromHudChart so hs cs = fromChartsWith so (runHud (getViewbox so cs) hs cs)

-- | render Charts with the supplied svg options.
fromChartsWith :: SvgOptions -> [Chart Double] -> Document
fromChartsWith so cs =
  (renderToDocument (getSize so cs'') r' cs'')
  where
    r' = r & maybe id padRect (so ^. #outerPad)
    cs'' =
      cs'
        & maybe id (\x -> frameChart x (fromMaybe 0 (so ^. #innerPad))) (so ^. #chartFrame)
    (r, cs') =
      bool
        (getViewbox so cs, cs)
        (scaleCharts (getViewbox so cs) cs)
        (ScaleCharts == so ^. #scaleCharts')

-- | render Charts to a Document using the supplied size and viewbox.
renderToDocument :: Point Double -> Rect Double -> [Chart Double] -> Document
renderToDocument (Point w' h') vb cs =
  Document
    ((\(Rect x z y w) -> Just (x, - w, z - x, w - y)) $ realToFrac <$> vb)
    (Just (Num (realToFrac w')))
    (Just (Num (realToFrac h')))
    (tree <$> cs)
    (unpack "")
    ""
    (PreserveAspectRatio False AlignNone Nothing)

-- * svg primitives
-- | convert a point to the svg co-ordinate system
-- The svg coordinate system has the y-axis going from top to bottom.
pointSvg :: (Real a) => Point a -> (Number, Number)
pointSvg (Point x y) = (Num (realToFrac x), Num (- (realToFrac y)))

-- | A DrawAttributes to rotate around a point by x degrees.
rotatePDA :: (Real a, HasDrawAttributes s) => a -> Point a -> s -> s
rotatePDA a (Point x y) s = s & transform %~ (Just . maybe r (<> r))
  where
    r = [Rotate (realToFrac a) (Just (realToFrac x, - realToFrac y))]

-- | A DrawAttributes to rotate by x degrees.
rotateDA :: (Real a, HasDrawAttributes s) => a -> s -> s
rotateDA a s = s & transform %~ (Just . maybe r (<> r))
  where
    r = [Rotate (realToFrac a) Nothing]

-- | A DrawAttributes to translate by a Point.
translateDA :: (Real a, HasDrawAttributes s) => Point a -> s -> s
translateDA p =
  transform
    %~ (\x -> Just $ maybe [Translate x' (- y')] (<> [Translate x' (- y')]) x)
  where
    Point x' y' = realToFrac <$> p

-- | convert a Rect to the svg co-ordinate system
rectSvg :: (Real a) => Rect a -> Rectangle -> Rectangle
rectSvg r =
  (rectUpperLeftCorner .~ (Num x, Num (- w)))
    . (rectWidth .~ Just (Num (z - x)))
    . (rectHeight .~ Just (Num (w - y)))
  where
    (Rect x z y w) = realToFrac <$> r

-- | Rectange svg
treeRect :: (Real a) => Rect a -> Tree
treeRect a =
  RectangleTree $ rectSvg a defaultSvg

-- | Text svg
treeText :: TextStyle -> Text -> Point Double -> Tree
treeText s t p =
  TextTree Nothing (textAt (pointSvg p) t) &
  maybe id (\x -> drawAttributes %~ rotatePDA x p) (realToFrac <$> s ^. #rotation)

-- | GlyphShape to svg Tree
treeShape :: GlyphShape -> Double -> Point Double -> Tree
treeShape CircleGlyph s p =
  CircleTree $ Circle mempty (pointSvg p) (Num (realToFrac s / 2))
treeShape SquareGlyph s p = treeRect (move p ((s *) <$> one))
treeShape (RectSharpGlyph x') s p =
  treeRect (move p (scale (Point s (x' * s)) one))
treeShape (RectRoundedGlyph x'' rx ry) s p =
  RectangleTree
    . rectSvg (addPoint p $ scale (Point s (x'' * s)) one)
    . (rectCornerRadius .~ (Just $ Num (realToFrac rx), Just $ Num (realToFrac ry)))
    $ defaultSvg
treeShape (TriangleGlyph (Point xa ya) (Point xb yb) (Point xc yc)) s p =
  PolygonTree
    . (polygonPoints .~ rps)
    $ (drawAttributes %~ translateDA p) defaultSvg
  where
    rps =
      [ V2 (s * xa) (- s * ya),
        V2 (s * xb) (- s * yb),
        V2 (s * xc) (- s * yc)
      ]
treeShape (EllipseGlyph x') s p =
  EllipseTree $
    Ellipse
      mempty
      (pointSvg p)
      (Num $ realToFrac s / 2)
      (Num $ (realToFrac x' * realToFrac s) / 2)
treeShape (VLineGlyph x') s (Point x y) =
  LineTree $
    Line
      (mempty & strokeWidth .~ Last (Just (Num (realToFrac x'))))
      (pointSvg (Point x (y - s / 2)))
      (pointSvg (Point x (y + s / 2)))
treeShape (HLineGlyph x') s (Point x y) =
  LineTree $
    Line
      (mempty & strokeWidth .~ Last (Just (Num $ realToFrac x')))
      (pointSvg (Point (x - s / 2) y))
      (pointSvg (Point (x + s / 2) y))
treeShape (PathGlyph path) _ p =
  PathTree $ Path ((drawAttributes %~ translateDA p) defaultSvg) path'
  where
    path' = either mempty (:[]) $ A.parseOnly command path

-- | GlyphStyle to svg Tree
treeGlyph :: (Fractional a, Real a) => GlyphStyle -> Point a -> Tree
treeGlyph s p =
  treeShape (s ^. #shape) (s ^. #size) (realToFrac <$> p)
    & maybe id (\x -> drawAttributes %~ rotatePDA x p) (realToFrac <$> s ^. #rotation)

-- | line svg
treeLine :: [Point Double] -> Tree
treeLine xs =
  PolyLineTree
    . (polyLinePoints .~ ((\(Point x y) -> V2 x (- y)) . fmap realToFrac <$> xs))
    $ defaultSvg

-- | convert a Chart to svg
tree :: Chart Double -> Tree
tree (Chart (TextA s ts) xs) =
  groupTrees (dagText s) (zipWith (treeText s) ts (toPoint <$> xs))
tree (Chart (GlyphA s) xs) =
  groupTrees (dagGlyph s) (treeGlyph s . toPoint <$> xs)
tree (Chart (LineA s) xs) =
  groupTrees (dagLine s) [treeLine (toPoint <$> xs)]
tree (Chart (RectA s) xs) =
  groupTrees (dagRect s) (treeRect <$> (toRect <$> xs))
tree (Chart (PixelA s) xs) =
  groupTrees (dagPixel s) (treeRect <$> (toRect <$> xs))
tree (Chart BlankA _) =
  groupTrees mempty []

-- | add drawing attributes as a group svg wrapping a [Tree]
groupTrees :: DrawAttributes -> [Tree] -> Tree
groupTrees da' tree' =
  GroupTree (drawAttributes %~ (<> da') $ groupChildren .~ tree' $ defaultSvg)

-- * DrawAttribute computations

-- | the extra Rect from the stroke element of an svg style attribute
strokeRect :: DrawAttributes -> Rect Double -> Rect Double
strokeRect das r = r + (realToFrac <$> Rect (- x / 2) (x / 2) (- x / 2) (x / 2))
  where
    x = case das ^. Svg.strokeWidth & getLast of
      Just (Num x') -> x'
      _ -> 0

data NotYetImplementedException = NotYetImplementedException deriving (Show)

instance Exception NotYetImplementedException

transformRect :: XY Double -> DrawAttributes -> Rect Double -> Rect Double
transformRect sp da r =
  realToFrac
    <$> foldl'
      addtr
      (realToFrac <$> r)
      (fromMaybe [] (da ^. transform))
  where
    (Point x y) = realToFrac <$> toPoint sp
    addtr r' t = case t of
      Translate x' y' -> move (Point x' (- y')) r'
      TransformMatrix {} ->
        throw NotYetImplementedException
      Scale s Nothing -> (s *) <$> r'
      Scale sx (Just sy) -> scale (Point sx sy) r'
      Rotate d Nothing ->
        rotateRect
          d
          ( realToFrac
              <$> move
                (Point (- x) (- y))
                (realToFrac <$> r')
          )
      Rotate d (Just (x', y')) -> rotateRect d (move (Point (x' - x) (y' - y)) (realToFrac <$> r'))
      SkewX _ -> throw NotYetImplementedException
      SkewY _ -> throw NotYetImplementedException
      TransformUnknown -> r'

dagRect :: RectStyle -> DrawAttributes
dagRect o =
  mempty
    & (strokeWidth .~ Last (Just $ Num (realToFrac $ o ^. #borderSize)))
    . (strokeColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #borderColor)))
    . (strokeOpacity ?~ realToFrac (opac $ o ^. #borderColor))
    . (fillColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #color)))
    . (fillOpacity ?~ realToFrac (opac $ o ^. #color))

dagPixel :: PixelStyle -> DrawAttributes
dagPixel o =
  mempty
    & (strokeWidth .~ Last (Just $ Num (realToFrac $ o ^. #pixelRectStyle . #borderSize)))
    . (strokeColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #pixelRectStyle . #borderColor)))
    . (strokeOpacity ?~ realToFrac (opac $ o ^. #pixelRectStyle . #borderColor))
    . (fillColor .~ Last (Just $ TextureRef (unpack $ o ^. #pixelTextureId)))

-- | group draw attributes for TextStyle. rotation is defined by svg relative to a point (or to an origin) and so rotation needs to be dealth with separately.
dagText :: () => TextStyle -> DrawAttributes
dagText o =
  mempty
    & (fontSize .~ Last (Just $ Num (o ^. #size)))
    & (strokeWidth .~ Last (Just $ Num 0))
    & (strokeColor .~ Last (Just FillNone))
    & (fillColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #color)))
    & (fillOpacity ?~ realToFrac (opac $ o ^. #color))
    & (textAnchor .~ Last (Just (toTextAnchor $ o ^. #anchor)))
    & maybe
      id
      (\(Point x y) -> transform ?~ [Translate (realToFrac x) (- realToFrac y)])
      (o ^. #translate)
  where
    toTextAnchor :: Anchor -> TextAnchor
    toTextAnchor AnchorMiddle = TextAnchorMiddle
    toTextAnchor AnchorStart = TextAnchorStart
    toTextAnchor AnchorEnd = TextAnchorEnd

dagGlyph :: GlyphStyle -> DrawAttributes
dagGlyph o =
  mempty
    & (strokeWidth .~ Last (Just $ Num (o ^. #borderSize)))
    & (strokeColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #borderColor)))
    & (strokeOpacity ?~ realToFrac (opac $ o ^. #borderColor))
    & (fillColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #color)))
    & (fillOpacity ?~ realToFrac (opac $ o ^. #color))
    & maybe
      id
      (\(Point x y) -> transform ?~ [Translate (realToFrac x) (- realToFrac y)])
      (o ^. #translate)

dagLine :: LineStyle -> DrawAttributes
dagLine o =
  mempty
    & (strokeWidth .~ Last (Just $ Num (o ^. #width)))
    . (strokeColor .~ Last (Just $ ColorRef (toPixelRGBA8 $ o ^. #color)))
    . (strokeOpacity ?~ realToFrac (opac $ o ^. #color))
    . (fillColor .~ Last (Just FillNone))

