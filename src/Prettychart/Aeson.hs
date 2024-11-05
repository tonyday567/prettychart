{-# LANGUAGE OverloadedStrings #-}

module Prettychart.Aeson where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

data GD = GD {isosamples:: [(Double,Double,Double)], gradd :: [[Double]], mom :: [[Double]], adam :: [[Double]]} deriving (Generic)

instance FromJSON GD where
  parseJSON = parseGD

parseGD :: Value -> Parser GD
parseGD = withObject "gradient descent" $ \obj -> do
  isosamples <- obj .: "isosamples"
  gd <- obj .: "gradient-descent"
  momentum <- obj .: "momentum"
  adam <- obj .: "adam"
  pure (GD isosamples gd momentum adam)
