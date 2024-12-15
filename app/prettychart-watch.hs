{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Watch a file or directory for svg to serve
module Main where

import Control.Concurrent.Async
import GHC.Generics
import Optics.Core
import Options.Applicative
import Prettychart
import Web.Rep.Socket
import Prelude
import Box

data Run = RunWatch deriving (Eq, Show)

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
  flag' RunWatch (long "watch" <> help "watch for svg chart files")
    <|> pure RunWatch

parseSocketConfig :: SocketConfig -> Parser SocketConfig
parseSocketConfig def =
  SocketConfig <$>
    option auto (value (view #host def) <> long "host" <> help "socket host") <*>
    option auto (value (view #port def) <> long "port" <> help "socket port") <*>
    option auto (value (view #path def) <> long "path" <> help "socket path")

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
    (fullDesc <> progDesc "SVG file server")

startWatcher :: IO ()
startWatcher = pure ()

startServer :: IO ()
startServer = pure ()

main :: IO ()
main = do
  o <- execParser (appConfig defaultAppConfig)
  let sc = appSocketConfig o
  let r = appRun o
  let fp = appFilePath o

  case r of
    RunWatch -> do
      (c, stop) <- displayFile sc (chartSocketPage Nothing)
      a <- async $ glue' c <$|> watchSvg fp
      putStrLn ("listening to" <> fp)
      print =<< wait a
      stop
