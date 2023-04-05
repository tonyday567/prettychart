{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Serve a chart web page with a web socket in it, that accepts 'ChartOptions'.
module Prettychart.Server
  ( startChartServer,
    startChartServerWith,
    printChart,
    chartSocketPage,
  )
where

import Box
import Chart
import Control.Concurrent.Async
import Control.Monad (when)
import Lucid as L
import Optics.Core
import Prettychart.Any
import Web.Rep

-- | 'Page' containing a web socket and javascript needed to run it.
chartSocketPage :: Page
chartSocketPage =
  bootstrap5Page
    & #jsOnLoad
      .~ mconcat
        [ webSocket,
          runScriptJs,
          refreshJsbJs
        ]
    & #htmlBody
      .~ divClass_
        "container"
        ( mconcat
            [ divClass_ "row" $ divClass_ "col" (h4_ "prettychart" <> L.with div_ [id_ "prettychart"] mempty)
            ]
        )

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
-- >> (sendChart, quitChartServer) <- startChartServer
-- >> sendChart unitExample
--
-- ... point browser to localhost:9160 ...
--
-- >> quitChartServer
startChartServer :: IO (ChartOptions -> IO Bool, IO ())
startChartServer = startChartServerWith defaultSocketConfig chartSocketPage

-- | Start the chart server protocol with bespoke 'SocketConfig' and 'Page' configurations.
--
-- > startChartServerWith (defaultSocketConfig & #port .~ 4567) (defaultSocketPage & #htmlBody %~ divClass_ "row" "bespoke footnote")
startChartServerWith :: SocketConfig -> Page -> IO (ChartOptions -> IO Bool, IO ())
startChartServerWith scfg page = do
  (Box c e, q) <- toBoxM Single
  x <- async $ serveSocketBox scfg page (Box toStdout (replace "prettychart" . renderChartOptions <$> e))
  pure (commit c, cancel x >> q)
