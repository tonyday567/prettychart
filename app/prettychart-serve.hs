{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Watch SVG files for changes, or run interactive demo
module Main where

import Chart
import Control.Concurrent.Async
import Chart.Examples
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forever)
import System.Process (callCommand)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics
import Optics.Core
import Options.Applicative
import Prettychart
import Prettychart.Server (ChartServerConfig (..), startChartServerHyperbole)
import Chart.Markup (writeChartOptions)
import System.FilePath
import System.FSNotify
import Web.Rep.Socket
import Prelude

data Run = RunWatch | RunDemo deriving (Eq, Show)

data AppConfig = AppConfig
  { appSocketConfig :: SocketConfig,
    appRun :: Run,
    appFilePath :: FilePath
  }
  deriving (Eq, Show, Generic)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig defaultSocketConfig RunWatch "."

parseRun :: Parser Run
parseRun =
  flag' RunDemo (long "demo" <> help "run interactive server demo")
    <|> flag' RunWatch (long "watch" <> help "watch for svg chart files")
    <|> pure RunWatch

parseSocketConfig :: SocketConfig -> Parser SocketConfig
parseSocketConfig def =
  SocketConfig
    <$> option auto (value (view #host def) <> long "host" <> help "socket host")
    <*> option auto (value (view #port def) <> long "port" <> help "socket port")
    <*> option auto (value (view #path def) <> long "path" <> help "socket path")

appParser :: AppConfig -> Parser AppConfig
appParser def =
  AppConfig
    <$> parseSocketConfig (view #appSocketConfig def)
    <*> parseRun
    <*> option str (value (view #appFilePath def) <> long "filepath" <> short 'f' <> help "file path to watch")

appConfig :: AppConfig -> ParserInfo AppConfig
appConfig def =
  info
    (appParser def <**> helper)
    (fullDesc <> progDesc "SVG file server and chart demo")

-- | Watch SVG file and serve updates
demoWatcher :: FilePath -> Int -> IO ()
demoWatcher fp port = do
  putStrLn $ "Starting file watcher on: " <> fp
  let cfg = ChartServerConfig port Nothing
  (send, stop) <- startChartServerHyperbole cfg

  -- Open browser automatically
  putStrLn "Opening browser to http://localhost:9160..."
  callCommand "open http://localhost:9160 &"
  -- Wait for server to settle
  threadDelay 5000000

  withManager $ \mgr -> do
    let onSvgChange :: Event -> IO ()
        onSvgChange e = case svgEvent e of
          Just svgPath -> do
            putStrLn $ "SVG file changed: " <> svgPath
            contents <- BL.readFile svgPath
            let text = TE.decodeUtf8 $ BL.toStrict contents
            _ <- send text
            pure ()
          Nothing -> pure ()

    _ <- watchDir mgr fp (isJust . svgEvent) onSvgChange
    putStrLn "Watching for SVG changes (press Ctrl-C to stop)..."
    threadDelay 1000000
  putStrLn "in main thread ..."
  forever $ threadDelay 1000000

-- | non-interactive server demo: start, open browser, cycle through examples
demoServer :: Int -> IO ()
demoServer port = do
  putStrLn "Starting interactive server demo..."
  let cfg = ChartServerConfig port Nothing
  (send, stop) <- startChartServerHyperbole cfg

  -- Open browser automatically
  putStrLn "Opening browser to http://localhost:9160..."
  callCommand "open http://localhost:9160 &"

  -- Wait for server to settle
  threadDelay 5000000

  putStrLn "Sending unitExample..."
  let unitHtml = renderChartOptions unitExample
  _ <- send unitHtml
  threadDelay 2000000
  putStrLn "(refresh browser to see chart)"

  putStrLn "Sending lineExample..."
  let lineHtml = renderChartOptions lineExample
  _ <- send lineHtml
  threadDelay 2000000

  putStrLn "Stopping server..."
  stop
  putStrLn "✓ Demo complete"


main :: IO ()
main = do
  o <- execParser (appConfig defaultAppConfig)
  let port = view #port (appSocketConfig o)
  let r = appRun o
  let fp = appFilePath o

  case r of
    RunWatch -> demoWatcher fp port
    RunDemo -> demoServer port
