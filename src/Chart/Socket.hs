{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GHC2021 #-}

-- | serve a chart page with a web socket in it.

module Chart.Socket where

import Chart
import Chart.Any
import Web.Rep
import Optics.Core
import Lucid as L
import Box
import Data.Text (pack)
import Control.Monad (when)
import Control.Concurrent.Async

chartSocketPage :: Page
chartSocketPage =
  bootstrap5Page &
  #jsOnLoad .~ mconcat
  [ webSocket,
    runScriptJs,
    refreshJsbJs,
    preventEnter
  ] &
  #htmlBody .~ divClass_ "container"
  ( mconcat
    [ divClass_ "row" (h1_ "any chart"),
      divClass_ "row" $ divClass_ "col" (h2_ "chart" <> L.with div_ [id_ "chart"] mempty)
    ])

printChart :: (Show a) => Committer IO ChartOptions -> Bool -> a -> IO ()
printChart c reprint s = case anyChart (pack $ show s) of
  Left _ -> print s
  Right co -> do
    b <- commit c co
    when (not b || reprint) (print s)

startServer :: IO (Committer IO ChartOptions, IO ())
startServer = do
  (Box c e, q) <- toBoxM Single
  x <- async $ serveSocketBox defaultSocketConfig chartSocketPage (Box toStdout (replace "chart" . renderChartOptions <$> e))
  pure (c, cancel x >> q)
