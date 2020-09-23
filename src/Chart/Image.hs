{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

-- | Munge a PNG into a [Chart Double]
--
module Chart.Image
  ( getImage,
    fromImage,
    colorChart,
    colorCrop,
  ) where

import Chart
import NumHask.Prelude hiding (fold)
import Data.List ((!!))
import Codec.Picture.Png
import Codec.Picture.Types
import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V

-- | turn a a PNG file into a chart
--
-- image is 372 x 436
--
-- >>> :set -XOverloadedLabels
-- >>> import Control.Lens
-- >>> i <- getImage "other/garfield.png"
-- >>> let r@(Rect x z y w) = (Rect (60::Int) 160 300 436)
-- >>> let svgo = defaultSvgOptions & #svgAspect .~ ManualAspect (fromIntegral (z-x) / fromIntegral (w-y))
-- >>> writeFile "other/garfield.svg" $ renderHudOptionsChart svgo defaultHudOptions [] (colorChart $ colorCrop r $ fromImage i)
--
-- as a png
-- ![png example](other/garfield.png)
--
-- as a chart
-- ![svg chart example](other/garfield.svg)
--
getImage :: FilePath -> IO (Image PixelRGB8)
getImage f = do
  bs <- B.readFile f
  pure $ either (const empty') extracti $ decodePng bs
    where
      extracti x = case x of
                     (ImageRGB8 i) -> i
                     _ -> empty'
      empty' = Image 0 0 V.empty

--  Rectangle chart made of single rectangles of colour.
colorChart :: [[Colour]] -> [Chart Double]
colorChart css = 
  (\(x, y) ->
      Chart (RectA (RectStyle 0 transparent ((css !! x) !! y)))
      [RectXY (Rect
               (fromIntegral x)
               (fromIntegral x+1)
               (fromIntegral y)
               (fromIntegral y + 1))]) <$> pts
  where
    h = length css
    w = maybe 0 length (head css)
    pts = (,) <$>
      (take h [(0 :: Int)..]) <*>
      (take w [(0 :: Int)..])

-- | reverses the top to bottom convention
colorCrop :: Rect Int -> [[Colour]] -> [[Colour]]
colorCrop (Rect x z y w) css = fmap (take (w - y) . drop y) . take (z - x) . drop x $ css

fromImage :: Image PixelRGB8 -> [[Colour]]
fromImage (Image w h v) =
  -- from top to bottom
  generate w (\x ->
    generate h (\y -> let i = (x + (h - y - 1) * w) * 3 in
                   Colour
                   (toFloat $ v V.! i)
                   (toFloat $ v V.! (i+1))
                   (toFloat $ v V.! (i+2))
                   1))
  where
    toFloat x = fromIntegral x / 256.0
    generate n f = f <$> (take n [0..])

