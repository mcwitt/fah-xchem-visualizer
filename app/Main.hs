{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import FahXChem.CompoundSeriesAnalysis
import FahXChem.Loader
import FahXChem.Server
import Options.Generic

data Command
  = Serve
  | Load FilePath
  deriving (Eq, Show, Generic)

instance ParseRecord Command

main :: IO ()
main = do
  cmd <- getRecord "fah-xchem visualizer"
  case cmd of
    Serve -> startServer 8080
    Load filePath -> do
      d <- load filePath
      case d of
        Right r -> do
          let compounds = csCompounds . series $ r
          print $ length compounds
          print $ take 5 compounds
        Left e -> error e
