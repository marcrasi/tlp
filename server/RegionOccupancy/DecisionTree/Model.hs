module RegionOccupancy.DecisionTree.Model where

import Import

import Data.Aeson
import Data.Aeson.Types
import Vision.Image.RGB.Type (RGB)

import ExceptHandler
import Image.Loader
import Polygon
import RegionOccupancy.Feature
import RegionOccupancy.OccupancyLabel

data DecisionTree
    = Decide OccupancyLabel
    | SwitchDotFrame Int Double DecisionTree DecisionTree
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
    } deriving (Show)

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
            o .: "regionId" <*>
            o .: "featureFrames" <*>
            o .: "decisionTree"

data LoadedModel = LoadedModel
    { regionId :: RegionId
    , features :: [Feature]
    , loadedTree :: DecisionTree
    }

unloadModel :: LoadedModel -> UnloadedModel
unloadModel (LoadedModel a b c) = UnloadedModel a (map featureFrame b) c

loadModel :: UnloadedModel -> ExceptHandler LoadedModel
loadModel (UnloadedModel regionId featureFrames tree) = do
    region <- (lift $ runDB $ get regionId) >>= getOrError "Could not find region."
    polygon <- getOrError "Could not decode polygon." $ decodeStrict $ encodeUtf8 $ regionValue region
    features <- mapM (loadFeature polygon) featureFrames
    return $ LoadedModel
        { regionId = regionId
        , features = features
        , loadedTree = tree
        }

loadFeature :: Polygon -> Entity Frame -> ExceptHandler Feature
loadFeature polygon (Entity frameId frame) = do
    unchoppedImage <- loadImage $ unpack $ frameFilename frame
    let choppedImage = chopImage polygon unchoppedImage
    return $ Feature (Entity frameId frame) choppedImage polygon
