
# Table of Contents

1.  [prettychart](#org52eda08)
2.  [Usage](#orgf05409a)
    1.  [live coding](#org062de09)
    2.  [ghci integration](#orgfcd7bd6)
3.  [Development](#orgf489981)
    1.  [live](#org2deed6a)
4.  [Prettychart.Any Examples](#orgca9d9d6)
    1.  [single list](#org9b83b20)
        1.  [10 or less elements => bar chart](#orgc860033)
        2.  [>1000 elements => histogram](#org7abe64c)
        3.  [< 1000 && > 10 => line chart](#orga634aff)
    2.  [double list](#org493d2b1)
        1.  [< 4 lists && < 10 values per list => bar chart](#org6e7fcb7)
        2.  [square => surface chart](#org5e9ba1b)
    3.  [tuple list [(Double, Double)] => scatter](#orgfc9b845)
    4.  [double tuple list [(Double, Double)] => scatter](#orgcd245d1)
    5.  [(Text, Double) tuple list](#org5cb6f57)


<a id="org52eda08"></a>

# prettychart

[![img](https://img.shields.io/hackage/v/prettychart.svg)](https://hackage.haskell.org/package/prettychart) [![img](https://github.com/tonyday567/prettychart/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/chart-svg/actions?query=workflow%3Ahaskell-ci)

This library contains:

-   A chart server, for use in conjunction with ghci, or other live coding situations. (See Chartpretty.Server)
-   anyChart, which attempts to convert text (of numbers) to a chart. (See Chartpretty.Any)
-   Some useful chart patterns that didn&rsquo;t make the cut in chart-svg (See Chartpretty.Charts)


<a id="orgf05409a"></a>

# Usage


<a id="org062de09"></a>

## live coding

The server can be used to view and develop chart-svg charts.

    > (display, quit) <- startChartServer (Just "prettychart")
    Setting phaser>s  to stun... (port 9160) (ctrl-c to quit)
    
    -- open localhost:9160
    -- developing and sending a chart to the server
    
    > import Chart.Examples
    > display lineExample
    
    > quit


<a id="orgfcd7bd6"></a>

## ghci integration

Add this to your .ghci.conf file to automatically go into :prettychart mode.

    -- :set -package prettychart
    :{
    :def! prettychart \_ -> pure $ unlines [
      "import Prettychart",
      "(sendChart, quitChartServer) <- startChartServer Nothing",
      "printc=printChart False sendChart",
      ":set -interactive-print printc"
      ]
    :}
    
    :{
    :def! noprettychart \_ -> pure $ unlines [
      "quitChartServer",
      ":set -interactive-print print"
      ]
    :}
    
    :prettychart

    ghci| ghci| ghci| ghci| ghci| ghci| ghci|
    > ghci| ghci| ghci| ghci| ghci|
    > Setting phasers to stun... (port 9160) (ctrl-c to quit)

    [1..200]

    :noprettychart


<a id="orgf489981"></a>

# Development

    :r
    :set prompt "> "
    :set -Wno-type-defaults
    :set -Wno-name-shadowing
    :set -XOverloadedStrings
    :set -XTupleSections
    :set -XOverloadedLabels
    import Chart hiding (quantiles)
    import Chart.Examples
    import Optics.Core
    import Prettychart.Charts
    import Prettychart.Any
    import Prettychart.ExampleData
    import Prettychart.Server
    import Data.Time.Calendar
    import qualified Data.Map as Map
    import Data.Text (Text,pack)
    import qualified Data.Text as Text
    import qualified Data.Text.IO as Text
    import Data.Time
    import qualified Data.List as List
    import Control.Category ((>>>))
    print "ok"

    [3 of 5] Compiling Prettychart.ExampleData ( src/Prettychart/ExampleData.hs, interpreted ) [Flags changed]
    [5 of 5] Compiling Prettychart      ( src/Prettychart.hs, interpreted ) [Flags changed]
    Ok, five modules loaded.
    >
    ok


<a id="org2deed6a"></a>

## live

    (display, quit) <- startChartServer Nothing

    display $ debugExample quadExample

    True


<a id="orgca9d9d6"></a>

# Prettychart.Any Examples


<a id="org9b83b20"></a>

## single list


<a id="orgc860033"></a>

### 10 or less elements => bar chart

    xs = [0..9]

    either Text.putStrLn (writeChartOptions "other/list1a.svg") $ anyChart (pack . show $ xs)

![img](other/list1a.svg)

    either Text.putStrLn (writeChartOptions "other/list1a.svg") $ anyChart (pack . show $ xs)


<a id="org7abe64c"></a>

### >1000 elements => histogram

    xs = sin <$> [0..2000]

    either Text.putStrLn (writeChartOptions "other/list1b.svg") $ anyChart (pack . show $ xs)

![img](other/list1b.svg)


<a id="orga634aff"></a>

### < 1000 && > 10 => line chart

In between goes for a line chartIn between goes for a line chart.

    xs = sin . (/100) <$> [0..500]

    either Text.putStrLn (writeChartOptions "other/list1c.svg") $ anyChart (pack . show $ xs)

![img](other/list1c.svg)


<a id="org493d2b1"></a>

## double list


<a id="org6e7fcb7"></a>

### < 4 lists && < 10 values per list => bar chart

    xs = [(1+) . sin <$> [0..8], (1+) . cos <$> [0..8]]
    xs

    [[1.0,1.8414709848078965,1.9092974268256817,1.1411200080598671,0.2431975046920718,4.1075725336861546e-2,0.7205845018010741,1.656986598718789,1.989358246623382],[2.0,1.5403023058681398,0.5838531634528576,1.0007503399554585e-2,0.34635637913638806,1.2836621854632262,1.960170286650366,1.7539022543433047,0.8544999661913865]]

    either Text.putStrLn (writeChartOptions "other/dlista.svg") $ anyChart (pack . show $ xs)

![img](other/dlista.svg)


<a id="org5e9ba1b"></a>

### square => surface chart

    iter2 f xs ys = f <$> xs <&> flip fmap ys -- or (\a -> f a <$> ys) <$> xs
    xs = iter2 (*) (fmap sin [1..20]) (fmap cos [1..20]) :: [[Double]]
    :t xs
    length xs
    fmap length xs

    xs :: [[Double]]
    20
    [20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20]

    either Text.putStrLn (writeChartOptions "other/dlistb.svg") $ anyChart (pack . show $ xs)

![img](other/dlistb.svg)


<a id="orgfc9b845"></a>

## tuple list [(Double, Double)] => scatter

    xs = zip (fmap (sin . (0.06*)) [1..100]) (fmap (cos . (0.06*)) [1..100])
    :t xs

    xs
      :: (TrigField b1, TrigField b2, Fractional b1, Fractional b2,
          Enum b1, Enum b2) =>
         [(b1, b2)]

    either Text.putStrLn (writeChartOptions "other/dtuple.svg") $ anyChart (pack . show $ xs)

![img](other/dtuple.svg)


<a id="orgcd245d1"></a>

## double tuple list [(Double, Double)] => scatter

    iter2 f xs ys = f <$> xs <&> flip fmap ys -- or (\a -> f a <$> ys) <$> xs
    
    
    xs = iter2 (\s (x,y) -> (s*x, s*y)) ((0.1*) <$> [1..10]) (zip (fmap (sin . (0.06*)) [1..100]) (fmap (cos . (0.06*)) [1..100]))
    :t xs

    > >
    xs :: (Fractional b, Enum b, TrigField b) => [[(b, b)]]

    either Text.putStrLn (writeChartOptions "other/dtupleb.svg") $ anyChart (pack . show $ xs)

![img](other/dtupleb.svg)


<a id="org5cb6f57"></a>

## (Text, Double) tuple list

    xs = (\x -> (show x, x)) <$> [0..9]

    either Text.putStrLn (writeChartOptions "other/tdtuple.svg") $ anyChart (pack . show $ xs)

![img](other/tdtuple.svg)

