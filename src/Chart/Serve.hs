{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Chart.Serve where

import Box
import Box.Socket
import Chart
import NumHask.Prelude
import Web.Rep
import Lucid as L
import Control.Lens

data SConfig =
  SConfig
  { framerate :: Double,
    runtime :: Double,
    s1 :: Double,
    s2 :: Double,
    numglyphs :: Int
  } deriving (Eq, Show, Generic)

repSConfig :: (Monad m) => SConfig -> SharedRep m SConfig
repSConfig s = SConfig <$> fr <*> mt <*> s1' <*> s2' <*> ng
  where
    fr = slider (Just "framerate") 1 200 1.0 (s ^. #framerate)
    mt = slider (Just "runtime") 0 1 0.1 (s ^. #runtime)
    s1' = slider (Just "s1") 0 1 0.1 (s ^. #s1)
    s2' = slider (Just "s2") 0 1 0.1 (s ^. #s2)
    ng = sliderI (Just "glyphs") 1 200 1 (s ^. #numglyphs)

totalFrames :: SConfig -> Int
totalFrames cfg = floor $ framerate cfg * runtime cfg

defaultSConfig :: SConfig
defaultSConfig = SConfig 20 10 1 1 100

run :: SConfig -> IO ()
run c = serveSend c (defaultAnimation c)

data Animation = Animation { freeze :: Double -> (HudOptions, [Chart Double]) }

instance Semigroup Animation where
  (<>) a b = Animation (\x -> (freeze a x) <> (freeze b x))

instance Monoid Animation where
  mempty = Animation (\_ -> (mempty,[]))
  mappend = (<>)

-- | charts
frameStamp :: Animation
frameStamp = Animation $ \x -> (mempty & #hudTitles .~ [defaultTitle (fixed (Just 3) x)], [])

defaultAnimation :: SConfig -> Animation
defaultAnimation c = mconcat $
    [ circle,
      circles c,
      frameStamp,
      stdHud (Rect -1 1 -1 1)
    ]

circle_ :: Double -> Point Double
circle_ x = Point (sin (2 * pi * x)) (cos (2 * pi * x))

circle :: Animation
circle = Animation $ \x -> (mempty, [Chart (GlyphA defaultGlyphStyle) [PointXY (circle_ x)]])

scale_ :: Double -> Point Double -> Point Double
scale_ s (Point x y) = Point (x*s) (y*s)

circles :: SConfig -> Animation
circles c = Animation $ \x ->
  (mempty,
   [ Chart
     (GlyphA defaultGlyphStyle)
     (PointXY . (\s -> scale_ s (circle_ (power x))) <$> xs)])
  where
    xs = grid InnerPos (Range 0 1) (c ^. #numglyphs)
    power x = (c ^. #s1) * x

-- | serveSend (defaultSConfig & #frameRate .~ 1000) frameStamp
serveSend :: SConfig -> Animation -> IO ()
serveSend cfg ani =
  serveSocketBox defaultSocketConfig chartPage . Box mempty <$.> chartEmitter cfg ani

-- | serveSend (defaultSConfig & #frameRate .~ 1000) frameStamp
serveCharts :: Emitter IO (HudOptions, [Hud Double], [Chart Double]) -> IO ()
serveCharts e =
  serveSocketBox defaultSocketConfig chartPage . Box mempty <$.> (pure (fmap toCodeText e))

-- | serveSend (defaultSConfig & #frameRate .~ 1000) frameStamp
serveSvgText :: Emitter IO Text -> IO ()
serveSvgText e =
  serveSocketBox defaultSocketConfig chartPage . Box mempty <$.> (pure (fmap toCode e))

toCode :: Text -> Text
toCode t =
  code (Replace "output" t)

cleanHud :: Rect Double -> [Chart Double]
cleanHud r = runHudWith r r (fst $ makeHud r defaultHudOptions) [Chart BlankA [RectXY r]]

stdHud :: Rect Double -> Animation
stdHud r = Animation (\_ -> (mempty, cleanHud r))

square :: Point (Double -> Double -> Double) -> SConfig -> Animation
square (Point fx fy) c =
  Animation (\x -> (mempty, [Chart (GlyphA defaultGlyphStyle) (PointXY <$> ps x)]))
  where
    ps x' = Point <$> (fx x' <$> xs) <*> (fy x' <$> xs)
    xs = grid InnerPos (Range 0 1) (c ^. #numglyphs)

line :: Point (Double -> Double -> Double) -> SConfig -> Animation
line (Point fx fy) c =
  Animation (\x -> (mempty, [Chart (GlyphA defaultGlyphStyle) (PointXY <$> ps x)]))
  where
    ps x' = zipWith Point (fx x' <$> xs) (fy x' <$> xs)
    xs = grid InnerPos (Range 0 1) (c ^. #numglyphs)

-- * wranglers
-- svgs <- runCont $ toListE <$> chartEmitter defaultSConfig (defaultAnimation defaultSConfig)
-- zipWithM_ (\x svg -> writeFile ("other/" <> show x <> ".svg") svg) [1..199] svgs
chartEmitter :: SConfig -> Animation -> Cont IO (Emitter IO Text)
chartEmitter c ani =
  fmap (outputText ani) <$> carousel c

outputText :: Animation -> Double -> Text
outputText ani x =
  code (Replace "output"
   (renderHudOptionsChart defaultSvgOptions (fst $ freeze ani x) [] (snd $ freeze ani x)))

toCodeText :: (HudOptions, [Hud Double], [Chart Double]) -> Text
toCodeText (ho,hs,cs) =
  code (Replace "output"
   (renderHudOptionsChart defaultSvgOptions ho hs cs))

-- | One of the joys of box is you get great support for adhoc low-level testing
--
-- > glue toStdout . fmap show <$.> carousel (defaultSConfig & #frameRate .~ 100)
carousel :: SConfig -> Cont IO (Emitter IO Double)
carousel cfg = delaylist ts xs
  where
    ts :: [Double]
    ts = replicate (totalFrames cfg) (1/cfg ^. #framerate)
    xs = grid InnerPos (Range 0 1) (totalFrames cfg)

delaylist :: [Double] -> [a] -> Cont IO (Emitter IO a)
delaylist ts xs = delay <$> fromListE ts <*> fromListE xs

delay :: Emitter IO Double -> Emitter IO a -> Emitter IO a
delay t e = Emitter $ do
  t' <- emit t
  e' <- emit e
  fromMaybe (pure ()) (sleep <$> t')
  pure e'

delaySecs :: Double -> Emitter IO a -> Emitter IO a
delaySecs t e = Emitter $ do
  e' <- emit e
  sleep t
  pure e'

chartPage :: Page
chartPage =
  bootstrapPage
    <> socketPage
    <> bodyPage

bodyPage :: Page
bodyPage =
  mempty & #htmlBody
    .~ divClass_
      "container"
      ( mconcat
          [ divClass_ "row" $ mconcat $ (\(t, h) -> divClass_ "col" (h2_ (toHtml t) <> L.with div_ [id_ t] h)) <$> [("output", mempty)]
          ]
      )
