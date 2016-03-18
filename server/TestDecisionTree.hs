import Application
import Import

import qualified Codec.Picture as C
import Data.Aeson
import Data.Time.Format
import Database.Persist.Sql (toSqlKey)
import qualified Vision.Image as I
import qualified Vision.Histogram as H
import qualified Vision.Image.JuicyPixels as J

import ExceptHandler
import Image.Normalization
import Polygon
import RegionOccupancy.Example
import RegionOccupancy.ExampleSelector
import RegionOccupancy.DecisionTree.Model
import RegionOccupancy.DecisionTree.Train

main = do
    result <- handler $ runExceptT testDecisionTree
    print $ show result

testDecisionTree :: ExceptHandler ()
testDecisionTree = do
    let regionId = toSqlKey 7

    trainExamples <- getExamples "2015-07-02 18:00:00" "2015-07-02 19:00:00" 200 regionId
    testExamples <- getExamples "2015-07-02 19:01:00" "2015-07-02 20:10:00" 200 regionId

    liftIO $ print ("Training examples: " ++ (show $ length trainExamples))
    liftIO $ print ("Testing examples: " ++ (show $ length testExamples))

    polygon <- getPolygon regionId

    let loadedModel = train defaultTrainOptions regionId polygon trainExamples

    liftIO $ print $ show $ unloadModel loadedModel

    liftIO $ print ("Train accuracy: " ++ (show $ computeAccuracy loadedModel trainExamples))
    liftIO $ print ("Test accuracy: " ++ (show $ computeAccuracy loadedModel testExamples))
    {-
    let unloadedModel = unloadModel loadedModel
    let modelValue = decodeUtf8 $ toStrict $ encode unloadedModel
    lift $ runDB $ insert $ DecisionTreeModel modelValue
    return modelValue
    -}

computeAccuracy :: LoadedModel -> [LabeledExample] -> Double
computeAccuracy model examples =
    (fromIntegral nCorrect) / (fromIntegral $ length examples)
  where
    nCorrect = sum $ map (\ex -> if decide model ex == getLabel ex then 1 else 0) examples

getPolygon :: RegionId -> ExceptHandler Polygon
getPolygon regionId = do
    region <- (lift $ runDB $ get regionId) >>= getOrError "Could not find region."
    getOrError "Could not decode polygon." $ decodeStrict $ encodeUtf8 $ regionValue region

getExamples :: String -> String -> Int -> RegionId -> ExceptHandler [LabeledExample]
getExamples startTimeString endTimeString count regionId = do
    let timeFormat = "%Y-%m-%d %T"
    startTime <- getOrError "Bad start time" $ parseTimeM True defaultTimeLocale timeFormat startTimeString
    endTime <- getOrError "Bad end time" $ parseTimeM True defaultTimeLocale timeFormat endTimeString
    let so = defaultSelectionOptions { startTime = startTime, endTime = endTime, exampleCount = count }
    selectExamples so regionId

doTN = do
    result <- handler $ runExceptT testNormalization
    print $ show result

testNormalization :: ExceptHandler ()
testNormalization = do
    let regionId = toSqlKey 7

    examples <- getExamples "2015-07-02 18:00:00" "2015-07-02 19:00:00" 5 regionId
    polygon <- getPolygon regionId

    let images = map ((chopImage polygon) . getImage) examples
    let equalized = map equalize images

    saveImages "exo_" (map maskToZero images)
    saveImages "exe_" (map maskToZero equalized)

saveImages :: FilePath -> [I.RGB] -> ExceptHandler ()
saveImages prefix images =
    mapM_ (\(index, i) -> saveImage (prefix ++ (show index) ++ ".png") i) (zip [1..] images)

saveImage :: FilePath -> I.RGB -> ExceptHandler ()
saveImage path i = do
    let rejuiced = C.ImageRGB8 $ J.toJuicyRGB i
    liftIO $ C.savePngImage path rejuiced
