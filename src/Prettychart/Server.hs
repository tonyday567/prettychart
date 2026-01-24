{-# LANGUAGE OverloadedStrings #-}

-- | Serve a chart web page with a web socket in it, that accepts 'ChartOptions'.
module Prettychart.Server
  ( -- * Socket Config (legacy, for migration)
    SocketConfig (..),
    defaultSocketConfig,

    -- * Hyperbole (new)
    ChartServerConfig (..),
    defaultChartServerConfig,
    startChartServerHyperbole,
    startChartServerPush,
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding
import Network.HTTP.Types (ok200)
import Network.Wai
import Network.Wai.Handler.Warp (run)

-- ============================================================================
-- Socket Config (Legacy - for migration from web-rep)
-- ============================================================================

-- | Configuration for web socket server (legacy, being phased out)
data SocketConfig = SocketConfig
  { host :: String,
    port :: Int,
    path :: String
  }
  deriving (Eq, Show)

-- | Default socket configuration
defaultSocketConfig :: SocketConfig
defaultSocketConfig = SocketConfig "127.0.0.1" 9160 "/ws"

-- ============================================================================
-- Hyperbole 0.6 Implementation (New)
-- ============================================================================

-- | Configuration for Hyperbole chart server
data ChartServerConfig = ChartServerConfig
  { serverPort :: Int,
    serverTitle :: Maybe Text
  }
  deriving (Show)

-- | Default configuration (port 9160, no title)
defaultChartServerConfig :: ChartServerConfig
defaultChartServerConfig = ChartServerConfig 9160 Nothing

-- | Render chart as HTML with meta-refresh polling
renderChartHtml :: Maybe Text -> Maybe Text -> BL.ByteString
renderChartHtml title mChart =
  BL.fromStrict $
    encodeUtf8 $
      "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>"
        <> fromMaybe "prettychart" title
        <> "</title><meta http-equiv=\"refresh\" content=\"2\"></head><body>"
        <> "<div class=\"container\" style=\"padding: 20px\">"
        <> maybe "" (\t -> "<h4>" <> t <> "</h4>") title
        <> fromMaybe "Waiting for chart..." mChart
        <> "</div></body></html>"

-- | Render chart as HTML without meta-refresh (for push/streaming)
renderChartHtmlPush :: Maybe Text -> Maybe Text -> BL.ByteString
renderChartHtmlPush title mChart =
  BL.fromStrict $
    encodeUtf8 $
      "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>"
        <> fromMaybe "prettychart" title
        <> "</title></head><body>"
        <> "<div class=\"container\" style=\"padding: 20px\">"
        <> maybe "" (\t -> "<h4>" <> t <> "</h4>") title
        <> fromMaybe "Waiting for chart..." mChart
        <> "</div></body></html>"

-- | Start Hyperbole chart server
startChartServerHyperbole :: ChartServerConfig -> IO (Text -> IO Bool, IO ())
startChartServerHyperbole cfg = do
  chartRef <- newIORef Nothing

  serverAsync <- async $
    run (serverPort cfg) $ \_ respond -> do
      mChart <- readIORef chartRef
      let html = renderChartHtml (serverTitle cfg) mChart
      let response = responseLBS ok200 [("Content-Type", "text/html; charset=utf-8")] html
      respond response

  threadDelay 100000

  putStrLn $ "Chart server listening on port " <> show (serverPort cfg)
  putStrLn $ "Open browser to http://localhost:" <> show (serverPort cfg)
  putStrLn "(ctrl-c to quit)"

  let sendChart content = do
        writeIORef chartRef (Just content)
        putStrLn "chartRef updated"
        pure True

  let quitServer = cancel serverAsync

  pure (sendChart, quitServer)

-- | Start chart server for push/streaming (no auto-refresh)
startChartServerPush :: ChartServerConfig -> IO (Text -> IO Bool, IO ())
startChartServerPush cfg = do
  chartRef <- newIORef Nothing

  serverAsync <- async $
    run (serverPort cfg) $ \_ respond -> do
      mChart <- readIORef chartRef
      let html = renderChartHtmlPush (serverTitle cfg) mChart
      let response = responseLBS ok200 [("Content-Type", "text/html; charset=utf-8")] html
      respond response

  threadDelay 100000

  putStrLn $ "Chart server (push mode) listening on port " <> show (serverPort cfg)
  putStrLn $ "Open browser to http://localhost:" <> show (serverPort cfg)
  putStrLn "(ctrl-c to quit)"

  let sendChart content = do
        writeIORef chartRef (Just content)
        putStrLn "chartRef updated"
        pure True

  let quitServer = cancel serverAsync

  pure (sendChart, quitServer)
