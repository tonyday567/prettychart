{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Watch SVG files for changes, or run interactive demo
module Main where

import Chart
import Chart.Examples
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forever, void, (<=<), forM_)
import Data.Text (Text, pack)
import Data.Text.IO qualified as TIO
import Data.Maybe (isJust)
import Data.Binary.Builder (fromByteString)
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import GHC.Generics
import Network.HTTP.Types (ok200)
import Network.Wai (Application, Response, ResponseReceived, pathInfo, responseLBS, responseStream)
import Network.Wai.Handler.Warp (run)
import Optics.Core
import Options.Applicative
import Prettychart
import System.FilePath
import System.FSNotify
import Prelude

data Run = RunWatch | RunDemo | RunPush deriving (Eq, Show)

data AppConfig = AppConfig
  { appPort :: Int,
    appRun :: Run,
    appFilePath :: FilePath,
    appWatchDir :: FilePath
  }
  deriving (Eq, Show, Generic)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig 9160 RunWatch "." "/tmp/watch/"

parseRun :: Parser Run
parseRun =
  flag' RunDemo (long "demo" <> help "run interactive server demo")
    <|> flag' RunPush (long "push" <> help "run SSE push test (60fps frame streaming)")
    <|> flag' RunWatch (long "watch" <> help "watch for svg chart files")
    <|> pure RunWatch

parsePort :: Int -> Parser Int
parsePort def =
  option auto (value def <> long "port" <> help "server port")

appParser :: AppConfig -> Parser AppConfig
appParser def =
  AppConfig
    <$> parsePort (view #appPort def)
    <*> parseRun
    <*> option str (value (view #appFilePath def) <> long "filepath" <> short 'f' <> help "file path to watch")
    <*> option str (value (view #appWatchDir def) <> long "watchdir" <> help "watch directory for SVG files (default: /tmp/watch/)")

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

-- | Counter chart generator: x -> ChartOptions
counterChart :: Int -> ChartOptions
counterChart x = mempty
  { chartTree = named (pack "counter")
      (pure $ TextChart defaultTextStyle [(pack (show x), zero)])
  }

-- | SSE push test: stream 50 counter charts at 10/sec (5 seconds total)
pushServer :: Int -> IO ()
pushServer port = do
  putStrLn $ "Starting SSE push test on port " <> show port
  putStrLn $ "Open browser to http://localhost:" <> show port <> "/push"

  -- Simple WAI application with /sse endpoint
  let app :: Application
      app _request respond = do
        case pathInfo _request of
          ["sse"] -> streamFrames respond
          _ -> serveIndexHtml respond

  putStrLn "Server started, press ctrl-c to stop"
  run port app

-- | Serve simple HTML page with EventSource
serveIndexHtml :: (Response -> IO ResponseReceived) -> IO ResponseReceived
serveIndexHtml respond = do
  let html = BL.fromStrict $ encodeUtf8 $
        "<!DOCTYPE html><html><head><title>SSE Push Test</title></head><body>" <>
        "<h1>SSE Push Test - Frame Stream</h1>" <>
        "<div id='count'>Frames received: 0</div>" <>
        "<div id='svg'></div>" <>
        "<script>" <>
        "let count = 0;" <>
        "const es = new EventSource('/sse');" <>
        "es.onmessage = (e) => { " <>
        "  count++; " <>
        "  document.getElementById('count').innerText = 'Frames received: ' + count; " <>
        "  document.getElementById('svg').innerHTML = e.data; " <>
        "}; " <>
        "es.onerror = (e) => console.error('SSE error', e);" <>
        "</script>" <>
        "</body></html>"
  respond $ responseLBS ok200
    [("Content-Type", "text/html; charset=utf-8")]
    html

-- | Stream frames via SSE at 100ms intervals (10/sec) for 50 frames
streamFrames :: (Response -> IO ResponseReceived) -> IO ResponseReceived
streamFrames respond = do
  let headers =
        [ ("Content-Type", "text/event-stream")
        , ("Cache-Control", "no-cache")
        , ("Connection", "keep-alive")
        ]

  let streamBody write flush = do
        putStrLn "Client connected, waiting 3 seconds before push..."
        threadDelay 3000000  -- 3 second startup delay
        putStrLn "Starting frame push..."

        forM_ [0..49] $ \i -> do
          let co = counterChart i
          let svg = encodeChartOptions co
          -- Remove newlines from SVG to send as single SSE message
          let svgOneLine = BS.filter (/= (10 :: Word8)) svg
          let frameBytes = BS.pack (map (fromIntegral . fromEnum) ("data: " :: String)) <> svgOneLine <> BS.pack (map (fromIntegral . fromEnum) ("\n\n" :: String))
          _ <- write (fromByteString frameBytes)
          _ <- flush
          threadDelay 100000  -- 100ms = 10 frames/sec

        putStrLn "✓ Push complete (50 frames sent)"

  respond $ responseStream ok200 headers streamBody

main :: IO ()
main = do
  o <- execParser (appConfig defaultAppConfig)
  let port = appPort o
  let r = appRun o
  let watchdir = appWatchDir o

  case r of
    RunWatch -> demoWatcher watchdir port
    RunDemo -> demoServer port
    RunPush -> pushServer port
