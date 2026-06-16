{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Watch SVG files for changes, or run interactive demo
module Main where

import Chart
import Chart.Examples
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forM_, forever, void, (<=<))
import Data.Binary.Builder (fromByteString)
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Maybe (isJust)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as TIO
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word8)
import GHC.Generics
import Network.HTTP.Types (ok200)
import Network.Wai (Application, Response, ResponseReceived, pathInfo, responseLBS, responseStream)
import Network.Wai.Handler.Warp (run)
import NumHask.Space (grid, Pos (LowerPos))
import Optics.Core
import Options.Applicative
import Prettychart
import System.FSNotify
import System.FilePath
import System.Process
import Prelude

data Run = RunWatch | RunDemo | RunPush | RunMouse | RunWheel deriving (Eq, Show)

data WheelParam = WLightness | WChroma | WHue
  deriving (Eq, Show, Read)

data AppConfig = AppConfig
  { appPort :: Int,
    appRun :: Run,
    appFilePath :: FilePath,
    appWatchDir :: FilePath,
    appPushSeconds :: Int,
    appChartsPerSecond :: Int,
    appWheelParam :: WheelParam
  }
  deriving (Eq, Show, Generic)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig 9160 RunWatch "." "/tmp/watch/" 5 10 WLightness

parseRun :: Parser Run
parseRun =
  flag' RunDemo (long "demo" <> help "run interactive server demo")
    <|> flag' RunPush (long "push" <> help "run SSE push test (frame streaming)")
    <|> flag' RunMouse (long "mouse" <> help "stream mouse location as glyph chart")
    <|> flag' RunWheel (long "wheel" <> help "animate color wheel slices via SSE")
    <|> flag' RunWatch (long "watch" <> help "watch for svg chart files")
    <|> pure RunWatch

parsePort :: Int -> Parser Int
parsePort def =
  option auto (value def <> long "port" <> help "server port")

parsePushSeconds :: Int -> Parser Int
parsePushSeconds def =
  option auto (value def <> long "push-seconds" <> help "duration of push test in seconds")

parseChartsPerSecond :: Int -> Parser Int
parseChartsPerSecond def =
  option auto (value def <> long "charts-per-second" <> help "charts per second for push test")

parseWheelParam :: WheelParam -> Parser WheelParam
parseWheelParam def =
  option
    (maybeReader readWheel)
    ( value def
        <> long "wheel-param"
        <> help "parameter to animate: lightness, chroma, or hue"
    )
  where
    readWheel "lightness" = Just WLightness
    readWheel "chroma"   = Just WChroma
    readWheel "hue"      = Just WHue
    readWheel _          = Nothing

appParser :: AppConfig -> Parser AppConfig
appParser def =
  AppConfig
    <$> parsePort (view #appPort def)
    <*> parseRun
    <*> option str (value (view #appFilePath def) <> long "filepath" <> short 'f' <> help "file path to watch")
    <*> option str (value (view #appWatchDir def) <> long "watchdir" <> help "watch directory for SVG files (default: /tmp/watch/)")
    <*> parsePushSeconds (view #appPushSeconds def)
    <*> parseChartsPerSecond (view #appChartsPerSecond def)
    <*> parseWheelParam (view #appWheelParam def)

appConfig :: AppConfig -> ParserInfo AppConfig
appConfig def =
  info
    (appParser def <**> helper)
    (fullDesc <> progDesc "SVG file server and chart demo")

onSvgChange :: (Text -> IO Bool) -> Event -> IO Bool
onSvgChange act e = maybe (pure False) ((True <$) . (act <=< TIO.readFile)) (svgEvent' e)

-- | Filter for svg file additions and modifications.
svgEvent' :: Event -> Maybe FilePath
svgEvent' (Added fp _ dir) = bool Nothing (Just fp) (takeExtension fp == ".svg" && dir == IsFile)
svgEvent' (Modified fp _ dir) = bool Nothing (Just fp) (takeExtension fp == ".svg" && dir == IsFile)
svgEvent' _ = Nothing

-- | Watch SVG file and serve updates
demoWatcher :: FilePath -> Int -> IO ()
demoWatcher watchdir port = do
  let cfg = ChartServerConfig port Nothing
  (send, _) <- startChartServerHyperbole cfg

  -- Start file watcher in background thread
  _ <- async $ withManager $ \mgr -> do
    _ <- watchDir mgr watchdir (isJust . svgEvent') (void . onSvgChange send)
    forever $ threadDelay 1000000

  forever $ threadDelay 1000000

-- | non-interactive server demo: start, open browser, cycle through examples
demoServer :: Int -> IO ()
demoServer port = do
  putStrLn "Starting interactive server demo..."
  let cfg = ChartServerConfig port Nothing
  (send, stop) <- startChartServerHyperbole cfg

  -- Wait for server to settle
  threadDelay 500000

  putStrLn "Sending unitExample..."
  let unitHtml = renderChartOptions unitExample
  _ <- send unitHtml
  threadDelay 2000000

  putStrLn "Sending lineExample..."
  let lineHtml = renderChartOptions lineExample
  _ <- send lineHtml
  threadDelay 2000000

  putStrLn "Stopping server..."
  stop
  putStrLn "✓ Demo complete"

-- | Get mouse location via Swift
getMouseLocation :: IO (Maybe (Double, Double))
getMouseLocation = do
  let swiftScript =
        "import Foundation\n\
        \import AppKit\n\
        \let location = NSEvent.mouseLocation\n\
        \print(\"\\(Int(location.x)),\\(Int(location.y))\")"
  result <- readProcess "swift" ["-"] swiftScript
  case words result of
    [coords] -> case break (== ',') coords of
      (xStr, ',' : yStr) -> do
        case (reads xStr :: [(Int, String)], reads yStr :: [(Int, String)]) of
          ([(x, "")], [(y, "")]) -> pure $ Just (fromIntegral x, fromIntegral y)
          _ -> pure Nothing
      _ -> pure Nothing
    _ -> pure Nothing

-- | Counter chart generator: x -> ChartOptions
counterChart :: Int -> ChartOptions
counterChart x =
  mempty
    { chartTree =
        named
          (pack "counter")
          (pure $ TextChart defaultTextStyle [(pack (show x), zero)])
    }

-- | Convert timestamp to opacity (1.0 = fresh, 0.0 = >10 seconds old)
timestampToOpacity :: UTCTime -> UTCTime -> Double
timestampToOpacity now ts =
  let age = realToFrac (diffUTCTime now ts) :: Double
      maxAge = 10.0 -- 10 seconds
   in max 0.0 (1.0 - (age / maxAge))

-- | Test mouse location reading
testMouseRead :: IO ()
testMouseRead = do
  putStrLn "Testing mouse location reading..."
  forM_ [1 .. 10 :: Int] $ \i -> do
    mLoc <- getMouseLocation
    case mLoc of
      Just (x, y) -> putStrLn $ "[" <> show i <> "] Mouse: " <> show (x, y)
      Nothing -> putStrLn $ "[" <> show i <> "] Failed to read mouse"
    threadDelay 100000

-- | SSE push test: stream counter charts
pushServer :: AppConfig -> IO ()
pushServer cfg = do
  let port = appPort cfg
  let seconds = appPushSeconds cfg
  let chartsPerSec = appChartsPerSecond cfg
  let numFrames = seconds * chartsPerSec
  let frameDelay = round (1000000.0 / fromIntegral chartsPerSec :: Double)

  putStrLn $ "Starting SSE push test on port " <> show port
  putStrLn $ "Configuration: " <> show seconds <> "s, " <> show chartsPerSec <> " charts/sec (" <> show numFrames <> " total frames)"
  putStrLn $ "Open browser to http://localhost:" <> show port

  -- Simple WAI application with /sse endpoint
  let app :: Application
      app _request respond = do
        case pathInfo _request of
          ["sse"] -> streamFrames respond numFrames frameDelay
          _ -> serveIndexHtml respond

  putStrLn "Server started, press ctrl-c to stop"
  run port app

-- | Serve simple HTML page with EventSource
serveIndexHtml :: (Response -> IO ResponseReceived) -> IO ResponseReceived
serveIndexHtml respond = do
  let html =
        BL.fromStrict $
          encodeUtf8 $
            "<!DOCTYPE html><html><head><title>SSE Push Test</title></head><body>"
              <> "<h1>SSE Push Test - Frame Stream</h1>"
              <> "<div id='count'>Frames received: 0</div>"
              <> "<div id='svg'></div>"
              <> "<script>"
              <> "let count = 0;"
              <> "const es = new EventSource('/sse');"
              <> "es.onmessage = (e) => { "
              <> "  count++; "
              <> "  document.getElementById('count').innerText = 'Frames received: ' + count; "
              <> "  document.getElementById('svg').innerHTML = e.data; "
              <> "}; "
              <> "es.onerror = (e) => console.error('SSE error', e);"
              <> "</script>"
              <> "</body></html>"
  respond $
    responseLBS
      ok200
      [("Content-Type", "text/html; charset=utf-8")]
      html

-- | Stream frames via SSE
streamFrames :: (Response -> IO ResponseReceived) -> Int -> Int -> IO ResponseReceived
streamFrames respond numFrames frameDelay = do
  let headers =
        [ ("Content-Type", "text/event-stream"),
          ("Cache-Control", "no-cache"),
          ("Connection", "keep-alive"),
          ("X-Accel-Buffering", "no")
        ]

  let streamBody write flush = do
        putStrLn "Client connected, waiting 3 seconds before push..."
        threadDelay 3000000 -- 3 second startup delay
        putStrLn "Starting frame push..."

        forM_ [0 .. numFrames - 1] $ \i -> do
          let co = counterChart i
          let svg = encodeChartOptions co
          -- Remove newlines from SVG to send as single SSE message
          let svgOneLine = BS.filter (/= (10 :: Word8)) svg
          let frameBytes = BS.pack (fmap (fromIntegral . fromEnum) ("data: " :: String)) <> svgOneLine <> BS.pack (fmap (fromIntegral . fromEnum) ("\n\n" :: String))
          _ <- write (fromByteString frameBytes)
          _ <- flush
          threadDelay frameDelay

        putStrLn $ "✓ Push complete (" <> show numFrames <> " frames sent)"
        putStrLn "Stream holding open with keep-alive..."
        forever $ do
          let keepAlive = BS.pack (fmap (fromIntegral . fromEnum) (": keep-alive\n\n" :: String))
          _ <- write (fromByteString keepAlive)
          _ <- flush
          threadDelay 30000000 -- 30 second keep-alive interval
  respond $ responseStream ok200 headers streamBody

-- | Mouse trail server: collect mouse positions and stream as glyph chart
mouseServer :: AppConfig -> IO ()
mouseServer cfg = do
  let port = appPort cfg
  let cps = appChartsPerSecond cfg
  let frameDelay = round (1000000.0 / fromIntegral cps :: Double)

  putStrLn $ "Starting mouse trail server on port " <> show port <> " at " <> show cps <> " cps"
  putStrLn $ "Open browser to http://localhost:" <> show port

  -- IORef to store mouse positions: [(x, y, timestamp)]
  trailRef <- newIORef ([] :: [(Double, Double, UTCTime)])

  -- Background thread: collect mouse positions at ~100Hz
  _ <- async $ forever $ do
    mLoc <- getMouseLocation
    case mLoc of
      Just (x, y) -> do
        now <- getCurrentTime
        modifyIORef trailRef $ \trail ->
          let trail' = (x, y, now) : trail
              -- Keep only points from last 10 seconds
              cutoff = 10.0 :: Double
              cutTrail = filter (\(_, _, ts) -> (realToFrac (diffUTCTime now ts) :: Double) <= cutoff) trail'
           in take 10000 cutTrail -- safety limit
      Nothing -> pure ()
    threadDelay 10000 -- 10ms = ~100Hz polling

  -- WAI app to serve mouse trail as HTML + SSE
  let app :: Application
      app _request respond = do
        case pathInfo _request of
          ["sse"] -> streamMouseTrail respond trailRef frameDelay
          _ -> serveMouseIndexHtml respond

  putStrLn "Server started, press ctrl-c to stop"
  run port app

-- | Serve HTML page with mouse trail EventSource
serveMouseIndexHtml :: (Response -> IO ResponseReceived) -> IO ResponseReceived
serveMouseIndexHtml respond = do
  let html =
        BL.fromStrict $
          encodeUtf8 $
            "<!DOCTYPE html><html><head><title>Mouse Trail</title></head><body>"
              <> "<h1>Mouse Trail Chart</h1>"
              <> "<div id='svg'></div>"
              <> "<script>"
              <> "const es = new EventSource('/sse');"
              <> "es.onmessage = (e) => { document.getElementById('svg').innerHTML = e.data; };"
              <> "es.onerror = (e) => console.error('SSE error', e);"
              <> "</script>"
              <> "</body></html>"
  respond $
    responseLBS
      ok200
      [("Content-Type", "text/html; charset=utf-8")]
      html

-- | Stream mouse trail as glyph chart via SSE
streamMouseTrail :: (Response -> IO ResponseReceived) -> IORef [(Double, Double, UTCTime)] -> Int -> IO ResponseReceived
streamMouseTrail respond trailRef frameDelay = do
  let headers =
        [ ("Content-Type", "text/event-stream"),
          ("Cache-Control", "no-cache"),
          ("Connection", "keep-alive"),
          ("X-Accel-Buffering", "no")
        ]

  let streamBody write flush = do
        putStrLn "Mouse trail client connected"
        forever $ do
          now <- getCurrentTime
          trail <- readIORef trailRef

          let glyphPoints = fmap (\(x, y, ts) -> GlyphChart (defaultGlyphStyle & set (#color % opac') (timestampToOpacity now ts)) [Point x y]) trail

          let co = mempty & set #chartTree (unnamed glyphPoints) & set #hudOptions defaultHudOptions
          let svg = encodeChartOptions co
          let svgOneLine = BS.filter (/= (10 :: Word8)) svg
          let frameBytes = BS.pack (fmap (fromIntegral . fromEnum) ("data: " :: String)) <> svgOneLine <> BS.pack (fmap (fromIntegral . fromEnum) ("\n\n" :: String))
          _ <- write (fromByteString frameBytes)
          _ <- flush
          threadDelay frameDelay

  respond $ responseStream ok200 headers streamBody

-- ============================================================================
-- Color Wheel Animation Server
-- ============================================================================

-- | Generate a single wheel frame for a given parameter at time t (seconds)
wheelFrame :: WheelParam -> Double -> ChartOptions
wheelFrame param t = case param of
  WLightness ->
    let l = 0.5 + 0.5 * sin (t * 2 * pi / 8)
    in wheelChart (rotatedWheelPoints 0) 0.01 50 l 0.5 (palette <$> [0 .. 7])
  WChroma ->
    let chroma = 0.1 + 0.9 * (0.5 + 0.5 * sin (t * 2 * pi / 8))
    in wheelChart (rotatedWheelPoints 0) 0.01 50 0.5 chroma (palette <$> [0 .. 7])
  WHue ->
    let hoffset = t * 36 -- ~1 full rotation per 10 seconds at 10 cps
    in wheelChart (rotatedWheelPoints hoffset) 0.01 50 0.5 0.5 (palette <$> [0 .. 7])

-- | Generate a color wheel chart from a wheel-point generator
wheelChart ::
  (Int -> Double -> Double -> [(Point Double, Colour)]) ->
  Double ->
  Int ->
  Double ->
  Double ->
  [Colour] ->
  ChartOptions
wheelChart wpGen s grain l maxchroma cs =
  mempty
    & set #hudOptions defaultHudOptions
    & set
      #chartTree
      ( named "bbox" [anchorRect]
          <> named "dots" (dotRef <$> cs)
          <> named "wheel" (dotWheel <$> filter (validColour . snd) (wpGen grain l maxchroma))
      )
  where
    anchorRect =
      RectChart
        (defaultRectStyle & set #color transparent & set #borderSize 0)
        [Rect (-0.3) 0.3 (-0.3) 0.3]
    dotRef x =
      let p = colour2Point x
       in GlyphChart
            ( defaultGlyphStyle
                & set #size 0.08
                & set #color x
                & set #borderColor (Colour 0.5 0.5 0.5 1)
                & set #glyphShape CircleGlyph
            )
            [p]
    colour2Point c = review lcha2colour' c & (\(LCHA _ ch h _) -> uncurry Point (review xy2ch' (ch, h)))
    dotWheel (p, c) =
      GlyphChart
        ( defaultGlyphStyle
            & set #size s
            & set #color c
            & set #borderSize 0
            & set #glyphShape CircleGlyph
        )
        [p]

-- | Generate wheel points (chroma-hue grid), optionally rotated by hoffset degrees
rotatedWheelPoints :: Double -> Int -> Double -> Double -> [(Point Double, Colour)]
rotatedWheelPoints hoffset grain l maxchroma =
  (\(Point c h) ->
    let h' = wrap360 (h + hoffset)
     in ( uncurry Point $ view (re xy2ch') (c, h'),
          view lcha2colour' (LCHA l c h' 1)
        )
    )
    <$> grid LowerPos (Rect 0 maxchroma 0 360) (Point grain grain)
  where
    wrap360 x = x - 360 * fromIntegral (floor (x / 360) :: Int)

-- | Serve color wheel animation via SSE
wheelServer :: AppConfig -> IO ()
wheelServer cfg = do
  let port = appPort cfg
  let cps = appChartsPerSecond cfg
  let param = appWheelParam cfg
  let frameDelay = round (1000000.0 / fromIntegral cps :: Double)

  putStrLn $ "Starting wheel animation on port " <> show port
  putStrLn $ "Parameter: " <> show param <> " at " <> show cps <> " fps"
  putStrLn $ "Open browser to http://localhost:" <> show port

  let app :: Application
      app _request respond = do
        case pathInfo _request of
          ["sse"] -> streamWheelFrames respond param frameDelay
          _ -> serveWheelIndexHtml respond param

  putStrLn "Server started, press ctrl-c to stop"
  run port app

-- | SSE stream of wheel frames
streamWheelFrames :: (Response -> IO ResponseReceived) -> WheelParam -> Int -> IO ResponseReceived
streamWheelFrames respond param frameDelay = do
  let headers =
        [ ("Content-Type", "text/event-stream"),
          ("Cache-Control", "no-cache"),
          ("Connection", "keep-alive"),
          ("X-Accel-Buffering", "no")
        ]

  let streamBody write flush = do
        putStrLn "Wheel client connected, starting animation..."
        threadDelay 1000000 -- 1 second startup delay

        let loop :: Double -> IO ()
            loop t = do
              let co = wheelFrame param t
              let svg = encodeChartOptions co
              let svgOneLine = BS.filter (/= (10 :: Word8)) svg
              let frameBytes =
                    BS.pack (fmap (fromIntegral . fromEnum) ("data: " :: String))
                      <> svgOneLine
                      <> BS.pack (fmap (fromIntegral . fromEnum) ("\n\n" :: String))
              _ <- write (fromByteString frameBytes)
              _ <- flush
              threadDelay frameDelay
              loop (t + fromIntegral frameDelay / 1000000.0)

        loop 0

  respond $ responseStream ok200 headers streamBody

-- | HTML page for wheel animation
serveWheelIndexHtml :: (Response -> IO ResponseReceived) -> WheelParam -> IO ResponseReceived
serveWheelIndexHtml respond param = do
  let paramLabel = case param of
        WLightness -> "Lightness"
        WChroma -> "Chroma"
        WHue -> "Hue"
  let html =
        BL.fromStrict $
          encodeUtf8 $
            "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>Color Wheel — "
              <> paramLabel
              <> "</title><style>"
              <> "body{background:#1a1a2e;color:#e0e0e0;font-family:system-ui,sans-serif;margin:0;display:flex;flex-direction:column;align-items:center;justify-content:center;min-height:100vh}"
              <> "h1{margin:0 0 8px 0;font-size:1.4rem;font-weight:400;letter-spacing:0.04em}"
              <> ".info{color:#888;font-size:0.85rem;margin-bottom:20px}"
              <> "#svg{background:#1a1a2e;border-radius:8px}"
              <> "#svg svg{display:block;max-width:90vw;max-height:80vh}"
              <> "</style></head><body>"
              <> "<h1>Color Wheel — "
              <> paramLabel
              <> " sweep</h1>"
              <> "<div class=\"info\">Animating "
              <> paramLabel
              <> " parameter</div>"
              <> "<div id=\"svg\"></div>"
              <> "<script>"
              <> "const es = new EventSource('/sse');"
              <> "es.onmessage = (e) => { document.getElementById('svg').innerHTML = e.data; };"
              <> "es.onerror = (e) => console.error('SSE error', e);"
              <> "</script>"
              <> "</body></html>"
  respond $
    responseLBS
      ok200
      [("Content-Type", "text/html; charset=utf-8")]
      html

main :: IO ()
main = do
  o <- execParser (appConfig defaultAppConfig)
  let port = appPort o
  let r = appRun o
  let watchdir = appWatchDir o

  case r of
    RunWatch -> demoWatcher watchdir port
    RunDemo -> demoServer port
    RunPush -> pushServer o
    RunMouse -> mouseServer o
    RunWheel -> wheelServer o
