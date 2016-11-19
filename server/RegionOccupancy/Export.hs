module RegionOccupancy.ExampleExporter where

import Import

import qualified Data.Aeson as DA

import qualified Image.Loader as IL
import qualified Polygon as P

exportLabeledExamples :: RegionId -> FilePath -> ExceptHandler ()
exportLabeledExamples regionId outputDirectory = do
    region <- (lift $ runDB $ get regionId) >>= getOrError "Region not found."
    let directionId = regionDirection region
    direction <- (lift $ runDB $ get directionId) >>= getOrError "Direction not found."

    polygon <- getOrError "Could not decode polygon." $ DA.decodeStrict $ encodeUtf8 $ regionValue region

    frames <- lift $ runDB $ selectList [FrameDirection ==. directionId] []
    labels <- lift $ runDB $ selectList [LabelRegion ==. regionId] []
    let labeledFrames = getLabeledFrames frames labels

    mapM_ (exportLabeledExample outputDirectory) labeledFrames


exportLabeledExample :: FilePath -> P.Polygon -> (OccupancyLabel, Entity Frame) -> ExceptHandler ()
extractOccupancyLabel outputDirectory (label, frame) = do
    image <- IL.loadImage $ unpack $ frameFilename frame


getLabeledFrames :: [Entity Frame] -> [Entity Label] -> [(OccupancyLabel, Entity Frame)]
getLabeledFrames frames labels =
    concatMap (toList . lookupOccupancyLabel labelMap) frames
  where
    labelMap = fromList $ concatMap (toList . extractOccupancyLabel) labels

