module Handler.DecisionTreeLabels where

import Import

import qualified Data.Map as M

import ExceptHandler
import Handler.ArgumentParser
import Handler.ResourceResponse
import RegionOccupancy.ExampleLoader
import RegionOccupancy.DecisionTree.Model
import RegionOccupancy.DecisionTree.Train
import RegionOccupancy.OccupancyLabel

data DecisionTreeLabel = DecisionTreeLabel
    { regionId :: RegionId
    , frameId :: FrameId
    , decisionTreeModelId :: DecisionTreeModelId
    , value :: OccupancyLabel
    }

instance ToJSON DecisionTreeLabel where
    toJSON (DecisionTreeLabel regionId frameId decisionTreeModelId value) =
        object
            [ "regionId" .= regionId
            , "frameId" .= frameId
            , "decisionTreeModelId" .= decisionTreeModelId
            , "value" .= value
            ]

getDecisionTreeLabelsR :: Handler Value
getDecisionTreeLabelsR = do
    maybeQuery <- getOptionalArgument "q"
    case maybeQuery of
        Nothing -> invalidArgs ["There is no getAll."]
        Just "decide" -> decideHandler
        Just otherQuery -> invalidArgs ["Unknown query '" ++ otherQuery ++ "'."]

decideHandler :: Handler Value
decideHandler = do
    frameId <- getArgument "frameId"
    example <- errorToInternalServer $ loadExample frameId

    decisionTreeModelId <- getArgument "decisionTreeModelId"
    model <- errorToInternalServer $ loadDecisionTreeByKey decisionTreeModelId

    let label = decide model example

    returnJson $ ResourceResponse
        { elements = [DecisionTreeLabel (loadedRegionId model) frameId decisionTreeModelId label]
        , linked = M.fromList []
        , pagination = Nothing
        }
