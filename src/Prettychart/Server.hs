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
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Text.Encoding
import MarkupParse
import Optics.Core hiding (element)
import Prettychart.Any
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
      .~ element "div" [Attr "class" "container"] (element "row" [Attr "class" "col"] (maybe mempty (elementc "h4" []) title) <> element_ "div" (pure $ Attr "id" "prettychart"))

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
