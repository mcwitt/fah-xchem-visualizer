{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module FahXChem.CompoundSeriesAnalysis where

import Data.Aeson
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

data CompoundSeriesAnalysisFile = CompoundSeriesAnalysisFile
  { asOf :: String,
    series :: CompoundSeriesAnalysis
  }
  deriving (Eq, Show, Generic)

instance FromJSON CompoundSeriesAnalysisFile where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance ToJSON CompoundSeriesAnalysisFile where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = camelTo2 '_'}

data CompoundSeriesAnalysis = CompoundSeriesAnalysis
  { csMetadata :: CompoundSeriesMetadata,
    csCompounds :: [CompoundAnalysis],
    csTransformations :: [TransformationAnalysis]
  }
  deriving (Eq, Show, Generic)

instance FromJSON CompoundSeriesAnalysis where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "cs"}

instance ToJSON CompoundSeriesAnalysis where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "cs"}

data CompoundSeriesMetadata = CompoundSeriesMetadata
  { csName :: Text,
    csDescription :: Text,
    csCreator :: Text,
    csCreatedAt :: Text,
    csXchemProject :: Text,
    csReceptorVariant :: Map String Text,
    csTemperatureKelvin :: Double,
    csIonicStrengthMillimolar :: Double,
    csPh :: Double,
    csFahProjects :: ProjectPair
  }
  deriving (Eq, Show, Generic)

compoundSeriesMetadataOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "csPh" -> "pH"
        s -> camelTo2 '_' . camelTo2 '_' . unprefixCamel "cs" $ s
    }

instance FromJSON CompoundSeriesMetadata where
  parseJSON = genericParseJSON compoundSeriesMetadataOptions

instance ToJSON CompoundSeriesMetadata where
  toEncoding = genericToEncoding compoundSeriesMetadataOptions

data ProjectPair = ProjectPair
  { ppComplexPhase :: Integer,
    ppSolventPhase :: Integer
  }
  deriving (Eq, Show, Generic)

instance FromJSON ProjectPair where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "pp"}

instance ToJSON ProjectPair where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "pp"}

data CompoundAnalysis = CompoundAnalysis
  { caMetadata :: CompoundMetadata,
    caMicrostates :: [MicrostateAnalysis],
    caFreeEnergy :: Maybe PointEstimate
  }
  deriving (Eq, Show, Generic)

instance FromJSON CompoundAnalysis where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "ca"}

instance ToJSON CompoundAnalysis where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "ca"}

data TransformationAnalysis = TransformationAnalysis
  { xfTransformation :: Transformation,
    xfReliableTransformation :: Bool,
    xfBindingFreeEnergy :: PointEstimate,
    xfExpDdg :: PointEstimate,
    xfAbsoluteError :: Maybe PointEstimate,
    xfComplexPhase :: PhaseAnalysis,
    xfSolventPhase :: PhaseAnalysis
  }
  deriving (Eq, Show, Generic)

instance FromJSON TransformationAnalysis where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "xf"}

instance ToJSON TransformationAnalysis where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "xf"}

data CompoundMetadata = CompoundMetadata
  { cCompoundId :: Text,
    cSmiles :: Text,
    cExperimentalData :: Map String Double
  }
  deriving (Eq, Show, Generic)

instance FromJSON CompoundMetadata where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "c"}

instance ToJSON CompoundMetadata where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "c"}

data MicrostateAnalysis = MicrostateAnalysis
  { msMicrostate :: Microstate,
    msFreeEnergy :: Maybe PointEstimate,
    msFirstPassFreeEnergy :: Maybe PointEstimate
  }
  deriving (Eq, Show, Generic)

instance FromJSON MicrostateAnalysis where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "ms"}

instance ToJSON MicrostateAnalysis where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "ms"}

data PointEstimate = PointEstimate
  { point :: Double,
    stderr :: Double
  }
  deriving (Eq, Show, Generic)

instance FromJSON PointEstimate

instance ToJSON PointEstimate

data Transformation = Transformation
  { xfRunId :: Int,
    xfXchemFragmentId :: String,
    xfInitialMicrostate :: CompoundMicrostate,
    xfFinalMicrostate :: CompoundMicrostate
  }
  deriving (Eq, Show, Generic)

instance FromJSON Transformation where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "xf"}

instance ToJSON Transformation where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "xf"}

data PhaseAnalysis = PhaseAnalysis
  { paFreeEnergy :: RelativeFreeEnergy,
    paGens :: [GenAnalysis]
  }
  deriving (Eq, Show, Generic)

instance FromJSON PhaseAnalysis where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "pa"}

instance ToJSON PhaseAnalysis where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "pa"}

data Microstate = Microstate
  { msMicrostateId :: Text,
    msFreeEnergyPenalty :: PointEstimate,
    msSmiles :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Microstate where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "ms"}

instance ToJSON Microstate where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "ms"}

data CompoundMicrostate = CompoundMicrostate
  { cmCompoundId :: String,
    cmMicrostateId :: String
  }
  deriving (Eq, Show, Generic)

instance FromJSON CompoundMicrostate where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "cm"}

instance ToJSON CompoundMicrostate where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = camelTo2 '_' . unprefixCamel "cm"}

data RelativeFreeEnergy = RelativeFreeEnergy
  { deltaF :: PointEstimate,
    barOverlap :: Double,
    numWorkPairs :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON RelativeFreeEnergy where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = camelTo2 '_'}

instance ToJSON RelativeFreeEnergy where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = camelTo2 '_'}

data GenAnalysis = GenAnalysis
  { gen :: Integer,
    genWorks :: [WorkPair],
    genFreeEnergy :: Maybe RelativeFreeEnergy
  }
  deriving (Eq, Show, Generic)

genAnalysisOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "gen" -> "gen"
        s -> camelTo2 '_' . unprefixCamel "gen" $ s
    }

instance FromJSON GenAnalysis where
  parseJSON = genericParseJSON genAnalysisOptions

instance ToJSON GenAnalysis where
  toEncoding = genericToEncoding genAnalysisOptions

data WorkPair = WorkPair
  { clone :: Int,
    forward :: Double,
    reverse :: Double
  }
  deriving (Eq, Show, Generic)

instance FromJSON WorkPair

instance ToJSON WorkPair

uncapitalize :: String -> String
uncapitalize "" = ""
uncapitalize (x : xs) = toLower x : xs

unprefixCamel :: String -> String -> String
unprefixCamel prefix s = uncapitalize . fromMaybe s $ stripPrefix prefix s
