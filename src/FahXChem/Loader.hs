{-# LANGUAGE OverloadedStrings #-}

module FahXChem.Loader where

import Control.Monad ((>=>))
import Data.Aeson
import Data.ByteString.Lazy as BS
import FahXChem.CompoundSeriesAnalysis

load :: FilePath -> IO (Either String CompoundSeriesAnalysisFile)
load = BS.readFile >=> pure . eitherDecode
