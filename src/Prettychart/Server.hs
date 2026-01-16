{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Serve a chart web page with a web socket in it, that accepts 'ChartOptions'.
module Prettychart.Server
  ( -- * Web-Rep (original)
    startChartServer,
    startChartServerWith,
    printChart,
    chartSocketPage,
    startFileServerWith,
    watchE,
    watchSvg,
    svgEvent,
    displayFile,
    -- * Hyperbole (new)
    ChartServerConfig (..),
    defaultChartServerConfig,
    startChartServerHyperbole,
  )
where

import Box
import Chart hiding (text)
import Control.Category ((>>>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad (forever, void, when)
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import MarkupParse qualified as MP
import Network.HTTP.Types (ok200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Optics.Core hiding (element)
import Prettychart.Any
import System.FSNotify
import System.FilePath
import Web.Rep

-- | 'Page' containing a web socket and javascript needed to run it.
chartSocketPage :: Maybe ByteString -> Page
chartSocketPage title =
  bootstrapPage
    & #jsOnLoad
    .~ mconcat
      [ webSocket,
        runScriptJs,
        refreshJsbJs
      ]
    & #htmlBody
    .~ MP.element "div" [MP.Attr "class" "container"] (MP.element "row" [MP.Attr "class" "col"] (maybe mempty (MP.elementc "h4" []) title) <> MP.element_ "div" (pure $ MP.Attr "id" "prettychart"))
    & #cssBody
    .~ cssColorScheme

-- | Print a chart supplying a 'ChartOptions' consumer, and a showable thing that may be chartable. The first argument flags whether to also print the item to stdout.
printChart :: (Show a) => Bool -> (ChartOptions -> IO Bool) -> a -> IO ()
printChart reprint send s = case anyChart (show s) of
  Left _ -> print s
  Right co -> do
    b <- send co
    when (not b || reprint) (print s)

-- | Start the chart server. Returns the chart consumer, and a server quit signal effect.
--
-- An iconic ghci session transcript:
--
-- >> import Chart.Examples
-- >> (sendChart, quitChartServer) <- startChartServer (Just "prettychart")
-- >> sendChart unitExample
--
-- ... point browser to localhost:9160 ...
--
-- >> quitChartServer
startChartServer :: Maybe String -> IO (ChartOptions -> IO Bool, IO ())
startChartServer title = startChartServerWith defaultSocketConfig (chartSocketPage $ pack <$> title)

-- | Start the chart server protocol with bespoke 'SocketConfig' and 'Page' configurations.
--
-- > startChartServerWith (defaultSocketConfig & #port .~ 4567) (defaultSocketPage & #htmlBody %~ divClass_ "row" "bespoke footnote")
startChartServerWith :: SocketConfig -> Page -> IO (ChartOptions -> IO Bool, IO ())
startChartServerWith scfg page = do
  (Box c e, q) <- toBoxM Single
  x <- async $ serveSocketBox scfg page (Box toStdout (decodeUtf8 . replace "prettychart" . encodeChartOptions <$> e))
  pure (commit c, cancel x >> q)

-- | Start a file server protocol with bespoke 'SocketConfig' and 'Page' configurations.
--
-- > startFileServerWith (defaultSocketConfig & #port .~ 4567) (defaultSocketPage & #htmlBody %~ divClass_ "row" "bespoke footnote")
startFileServerWith :: SocketConfig -> Page -> IO (FilePath -> IO Bool, IO ())
startFileServerWith scfg page = do
  (Box c e, q) <- toBoxM Single
  x <- async $ serveSocketBox scfg page (Box toStdout (decodeUtf8 . replace "prettychart" <$> witherE (B.readFile >>> fmap Just) e))
  pure (commit c, cancel x >> q)

-- | Emit from the fsnotify watch manager.
watchE :: [Char] -> Codensity IO (Emitter IO Event)
watchE fp =
  emitQ
    New
    ( \c -> withManager $ \mgr -> do
        putStrLn "watchDir started"
        _ <- watchDir mgr fp (const True) (void . commit c)
        _ <- forever $ threadDelay 1000000
        -- never gets to here:
        putStrLn "watchDir stopped"
    )

-- | Emit from the fsnotify watch manager.
-- > glue' toStdout <$|> fmap (Text.pack . show) <$> watchE "."
watchSvg :: FilePath -> CoEmitter IO FilePath
watchSvg fp =
  emitQ
    New
    ( \c -> withManager $ \mgr -> do
        putStrLn "watchDir started"
        _ <- watchDir mgr fp (isJust . svgEvent) (maybe (pure ()) (void . commit c) . svgEvent)
        _ <- forever $ threadDelay 1000000
        -- never gets to here:
        putStrLn "watchDir stopped"
    )

-- | Filter for svg file additions and modifications.
svgEvent :: Event -> Maybe FilePath
svgEvent (Added fp _ dir) = bool Nothing (Just fp) (takeExtension fp == ".svg" && dir == IsFile)
svgEvent (Modified fp _ dir) = bool Nothing (Just fp) (takeExtension fp == ".svg" && dir == IsFile)
svgEvent _ = Nothing

-- | Create a file committer to serve an SVG file.
displayFile :: SocketConfig -> Page -> IO (Committer IO FilePath, IO ())
displayFile scfg page = do
  (Box c e, q) <- toBoxM Single
  x <- async $ serveSocketBox scfg page (Box toStdout (decodeUtf8 . replace "prettychart" <$> e))
  pure (witherC (B.readFile >>> fmap Just) c, cancel x >> q)

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
renderChartHtml :: Maybe Text -> Maybe ChartOptions -> BL.ByteString
renderChartHtml title mChart =
  BL.fromStrict $ encodeUtf8 $
    "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>" <>
    fromMaybe "prettychart" title <>
    "</title><meta http-equiv=\"refresh\" content=\"2\"></head><body>" <>
    "<div class=\"container\" style=\"padding: 20px\">" <>
    maybe "" (\t -> "<h4>" <> t <> "</h4>") title <>
    (case mChart of
      Just co -> renderChartOptions co
      Nothing -> "Waiting for chart...") <>
    "</div></body></html>"

-- | Start Hyperbole chart server
startChartServerHyperbole :: ChartServerConfig -> IO (ChartOptions -> IO Bool, IO ())
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

  let sendChart co = do
        writeIORef chartRef (Just co)
        putStrLn "Chart updated - refresh browser to see changes"
        pure True

  let quitServer = cancel serverAsync

  pure (sendChart, quitServer)
