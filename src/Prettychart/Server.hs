{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | serve a chart page with a web socket in it.
module Prettychart.Server
  ( printChart,
    startChartServer,
    startChartServerWith,
    chartSocketPage,
  )
where

import Box
import Chart
import Control.Concurrent.Async
import Control.Monad (when)
import Data.Text (pack)
import Lucid as L
import Optics.Core
import Prettychart.Any
import Web.Rep

-- | web-rep Page containing a web socket and javascript needed to run it.
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

-- | print a chart supplying a consumption hook, and a showable thing that may be something able to be charted. The first argument flags whether to also print the item to stdout.
printChart :: (Show a) => Bool -> (ChartOptions -> IO Bool) -> a -> IO ()
printChart reprint send s = case anyChart (pack $ show s) of
  Left _ -> print s
  Right co -> do
    b <- send co
    when (not b || reprint) (print s)

-- | Start the chart server. Returns the chart consumer, and a server quit signal effect.
--
-- > import Chart.Examples
-- > (sendChart, quitChartServer) <- startChartServer
-- > sendChart unitExample
-- ... point browser to localhost:9160 ...
-- > quitChartServer
startChartServer :: IO (ChartOptions -> IO Bool, IO ())
startChartServer = startChartServerWith defaultSocketConfig chartSocketPage

-- | start the chart server protocol with specific configurations.
--
-- > startChartServerWith (defaultSocketConfig & #port .~ 4567) (defaultSocketPage & #htmlBody %~ divClass_ "row" "bespoke footnote")
startChartServerWith :: SocketConfig -> Page -> IO (ChartOptions -> IO Bool, IO ())
startChartServerWith scfg page = do
  (Box c e, q) <- toBoxM Single
  x <- async $ serveSocketBox scfg page (Box toStdout (replace "prettychart" . renderChartOptions <$> e))
  pure (commit c, cancel x >> q)
