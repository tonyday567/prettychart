{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Watch SVG files for changes, or run interactive demo
module Main where

import Chart
import Chart.Examples
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forever, void, (<=<))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Maybe (isJust)
import Data.Bool
import GHC.Generics
import Optics.Core
import Options.Applicative
import Prettychart
import System.FilePath
import System.FSNotify
import Prelude

data Run = RunWatch | RunDemo deriving (Eq, Show)

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


main :: IO ()
main = do
  o <- execParser (appConfig defaultAppConfig)
  let port = appPort o
  let r = appRun o
  let watchdir = appWatchDir o

  case r of
    RunWatch -> demoWatcher watchdir port
    RunDemo -> demoServer port
