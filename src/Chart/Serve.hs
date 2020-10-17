{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}

-- | Serve 'Chart's using [web-rep](http://hackage.haskell.org/package/web-rep) technology.
module Chart.Serve
  ( serveCharts,
    serveSvgText,
    chartPage,
  )
where

import Box
import Box.Socket
import Chart
import Control.Lens
import Lucid as L
import NumHask.Prelude
import Web.Rep

-- | Serve a 'Chart' 'Emitter' using web-rep.
serveCharts :: Emitter IO (HudOptions, [Hud Double], [Chart Double]) -> IO ()
serveCharts e =
  serveSocketBox defaultSocketConfig chartPage . Box mempty <$.> (pure (fmap toCodeText e))

-- | Serve a Text (typically SVG) 'Emitter' using web-rep.
serveSvgText :: Emitter IO Text -> IO ()
serveSvgText e =
  serveSocketBox defaultSocketConfig chartPage . Box mempty <$.> (pure (fmap toCode e))

toCode :: Text -> Text
toCode t =
  code (Replace "output" t)

toCodeText :: (HudOptions, [Hud Double], [Chart Double]) -> Text
toCodeText (ho, hs, cs) =
  code
    ( Replace
        "output"
        (renderHudOptionsChart defaultSvgOptions ho hs cs)
    )

-- | A static html page to output a chart.
chartPage :: Page
chartPage =
  bootstrapPage
    <> socketPage
    <> bodyPage

bodyPage :: Page
bodyPage =
  mempty & #htmlBody
    .~ divClass_
      "container"
      ( mconcat
          [ divClass_ "row" $ mconcat $ (\(t, h) -> divClass_ "col" (h2_ (toHtml t) <> L.with div_ [id_ t] h)) <$> [("output", mempty)]
          ]
      )
