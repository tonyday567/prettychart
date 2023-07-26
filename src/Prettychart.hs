{-# OPTIONS_HADDOCK prune #-}

-- | A haskell library to serve charts via ghci.
module Prettychart
  ( -- * Usage

    --
    -- $usage

    -- * Re-exports
    module Prettychart.Server,
    module Prettychart.Any,
    module Prettychart.Charts,
  )
where

import Prettychart.Any
import Prettychart.Charts
import Prettychart.Server

-- $usage
--
-- >> -- :set -package prettychart
-- >> import Prettychart
-- >> (sendChart, quitChartServer) <- startChartServer
--
-- An example from chart-svg ...
--
-- >> import Chart.Examples
-- >> sendChart lineExample
--
-- ... point your browser at localhost:9160 and you should see this:
--
-- ![line Example](other/line.svg)
--
-- Utilising -interactive-print, we can turn these protocols into a pretty printer for collections of numbers:
--
-- >> printc = printChart False sendChart
-- >> :t printc
-- >> (Show a) => a -> IO ()
-- >> :set -interactive-print printc
-- >> [0..9]
--
-- A bar chart representing this list will be rendered in the browser, and should look like this:
--
-- ![list Example](other/list1a.svg)
--
-- ... stop the server, and go back to normal.
--
-- >> quitChartServer
-- >> :set -interactive-print print
--
-- All of this can be automated by dropping the code below into a ghci.conf
--
-- > -- :set -package prettychart
-- > :{
-- > :def! prettychart \_ -> pure $ unlines [
-- >   "import Prettychart",
-- >   "(sendChart, quitChartServer) <- startChartServer",
-- >   "printc=printChart False sendChart",
-- >   ":set -interactive-print printc"
-- >   ]
-- > :}
-- >
-- > :{
-- > :def! noprettychart \_ -> pure $ unlines [
-- >   "quitChartServer",
-- >   ":set -interactive-print print"
-- >   ]
-- > :}
-- >
-- > :prettychart
-- >
