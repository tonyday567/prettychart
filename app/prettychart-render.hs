{-# LANGUAGE OverloadedStrings #-}

-- | Render a .chart file to SVG.
--
-- A .chart file is a serialized 'ChartOptions' value (produced by 'show').
-- Usage:
--   prettychart-render input.chart               -- writes input.svg
--   prettychart-render input.chart -o out.svg     -- writes out.svg
--   prettychart-render input.chart -p 9160        -- serves in browser
module Main where

import Chart
import Data.Bool
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types (ok200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Options.Applicative
import System.FilePath (dropExtension, takeFileName, (<.>))
import Prelude

data Mode = WriteFile FilePath | Serve Int
  deriving (Show)

data Config = Config
  { inputFile :: FilePath,
    mode :: Mode
  }
  deriving (Show)

parseConfig :: Parser Config
parseConfig =
  Config
    <$> argument str (metavar "FILE" <> help ".chart file to render")
    <*> ( flag' (Serve 9160) (long "serve" <> short 's' <> help "serve in browser on port 9160")
            <|> (Serve <$> option auto (long "port" <> short 'p' <> metavar "PORT" <> help "serve on specified port"))
            <|> ( WriteFile
                    <$> option
                      str
                      (long "output" <> short 'o' <> metavar "FILE" <> help "output SVG file (default: <input>.svg)")
                )
            <|> pure (WriteFile "")
        )

main :: IO ()
main = do
  config <- execParser (info (parseConfig <**> helper) (fullDesc <> progDesc "Render a .chart file to SVG"))
  chartText <- readFile (inputFile config)
  let chart = read chartText :: ChartOptions
  case mode config of
    WriteFile out -> do
      let fp = bool (dropExtension (inputFile config) <.> "svg") out (null out)
      writeChartOptions fp chart
      putStrLn $ "Wrote " <> fp
    Serve port -> do
      putStrLn $ "Serving on http://localhost:" <> show port
      putStrLn $ "File: " <> takeFileName (inputFile config)
      let svg = decodeUtf8 (encodeChartOptions chart)
      let app :: Application
          app _req respond = do
            let html =
                  BL.fromStrict $
                    encodeUtf8 $
                      "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>"
                        <> (Text.pack . takeFileName . inputFile $ config)
                        <> "</title><style>body{background:#fff;margin:0;display:flex;justify-content:center;align-items:center;min-height:100vh}"
                        <> "#svg svg{max-width:95vw;max-height:95vh}</style></head><body>"
                        <> "<div id=\"svg\">"
                        <> svg
                        <> "</div></body></html>"
            respond $
              responseLBS ok200 [("Content-Type", "text/html; charset=utf-8")] html
      run port app
