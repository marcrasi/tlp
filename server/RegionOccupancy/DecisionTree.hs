module RegionOccupancy.DecisionTree where

import Import

import Data.Aeson
import Data.Aeson.Types
import Vision.Image.RGB.Type (RGB)

import Polygon
import RegionOccupancy.OccupancyLabel

data DecisionTree
    = Decide OccupancyLabel
    | SwitchDotFrame Int Float DecisionTree DecisionTree
    deriving (Show)

instance ToJSON DecisionTree where
    toJSON (Decide occupancyLabel) =
        object [ "decide" .= occupancyLabel ]
    toJSON (SwitchDotFrame index threshold lower higher) =
        object
            [ "switchDotFrame" .= index
            , "threshold" .= threshold
            , "lower" .= lower
            , "higher" .= higher
            ]

decideParser :: Value -> Parser DecisionTree
decideParser (Object o) = Decide <$> o .: "decide"
decideParser _ = mzero

switchDotFrameParser :: Value -> Parser DecisionTree
switchDotFrameParser (Object o) =
    SwitchDotFrame <$>
        o .: "switchDotFrame" <*>
        o .: "threshold" <*>
        o .: "lower" <*>
        o .: "higher"
switchDotFrameParser _ = mzero

instance FromJSON DecisionTree where
    parseJSON value =
       decideParser value <|> switchDotFrameParser value

data UnloadedModel = UnloadedModel
    { unloadedRegionId :: RegionId
    , unloadedFeatureFrames :: [Entity Frame]
    , unloadedTree :: DecisionTree
    }

instance ToJSON UnloadedModel where
    toJSON (UnloadedModel regionId featureFrames tree) =
        object
            [ "regionId" .= regionId
            , "featureFrames" .= featureFrames
            , "decisionTree" .= tree
            ]

instance FromJSON UnloadedModel where
    parseJSON (Object o) =
        UnloadedModel <$>
            o .: "regionId"
            o .: "featureFrames"
            o .: "decisionTree"

data LoadedModel = LoadedModel
    { regionId :: RegionId
    , featurePolygon :: Polygon
    , featureImages :: [RGB]
    , featureFrames :: [Entity Frame]
    , loadedTree :: DecisionTree
    }

unloadModel :: LoadedModel -> UnloadedModel
unloadModel (LoadedModel a b c d e) = UnloadedModel a d e

loadModel :: UnloadedModel -> Handler (Maybe LoadedModel)
loadModel (UnloadedModel regionId featureFrames tree) = do
    regionMay <- get regionId
    let
