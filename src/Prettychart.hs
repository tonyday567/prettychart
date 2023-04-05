{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK prune #-}

-- | A haskell library to serve charts via ghci.
module Prettychart
  ( -- * Usage

    --
    -- $usage

    -- * Re-exports
    module Prettychart.Server,
    module Prettychart.Any,
    module Prettychart.Charts
  )
where

import Prettychart.Server
import Prettychart.Any
import Prettychart.Charts

-- $usage
--
-- > :set -XOverloadedStrings
-- > import Prettychart
-- > (sendChart, quitChartServer) <- startChartServer
--
-- From chart-svg ...
-- > import Chart.Examples
-- > sendChart $ lineExample
--
-- Will render the chart in your browser at localhost:9160
--
-- > printc = printChart False sendChart
-- > :t printc
-- (Show a) => a -> IO ()
--
-- > :set -interactive-print printc
--
-- > [1..7]
--
-- A bar chart will be rendered in the browser.
--
-- To stop the server, and go back to normal.
-- > quitChartServer
-- > :set -interactive-print print
