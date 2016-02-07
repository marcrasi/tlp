import Application
import Import

import Data.Aeson
import Database.Persist.Sql (toSqlKey)

import ExceptHandler
import RegionOccupancy.ExampleSelector
import RegionOccupancy.DecisionTree.Model
import RegionOccupancy.DecisionTree.Train

main = do
    result <- exceptHandler testDecisionTree
    print $ show result

testDecisionTree :: ExceptHandler Text
testDecisionTree = do
    currentTime <- liftIO $ getCurrentTime
    let so = defaultSelectionOptions { endTime = currentTime, exampleCount = 400 }
    let regionId = toSqlKey 7
    region <- (lift $ runDB $ get regionId) >>= getOrError "Could not find region."
    polygon <- getOrError "Could not decode polygon." $ decodeStrict $ encodeUtf8 $ regionValue region
    examples <- selectExamples so regionId
    let loadedModel = train defaultTrainOptions regionId polygon examples
    let unloadedModel = unloadModel loadedModel
    let modelValue = decodeUtf8 $ toStrict $ encode unloadedModel
    lift $ runDB $ insert $ DecisionTreeModel modelValue
    return modelValue
