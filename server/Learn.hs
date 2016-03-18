module Learn where

import Application
import Import

import Codec.Picture
import Codec.Picture.Types
import Database.Persist.Sql (toSqlKey)
import Data.Aeson (decode)
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Text.Lazy as LT
import qualified Data.Vector.Storable as V
import System.IO (withBinaryFile, IOMode(ReadMode))
import Vision.Image.Conversion
import Vision.Image.Filter
import Vision.Image.Grey.Type
import Vision.Image.JuicyPixels
import Vision.Image.Type

import qualified Data.List as L

import Polygon

indexWithDefault :: Int -> a -> [a] -> a
indexWithDefault index def list =
    case headMay $ drop index list of
      Nothing -> def
      Just value -> value

data DecisionTree
    = Decide Int
    | Switch Int Double DecisionTree DecisionTree
    deriving Show

decide :: DecisionTree -> ProcessedExample -> Int
decide (Decide decision) _ = decision
decide (Switch index value low high) example =
    if getFeatureValue index example < value
      then decide low example
      else decide high example

empericalP :: (Eq a) => [a] -> a -> Double
empericalP list value =
    (sum $ map (\a -> if a == value then 1.0 else 0.0) list) / (fromIntegral $ length list)

splitGoodness :: [ProcessedExample] -> Int -> Double -> Double
splitGoodness examples featureIndex split =
    let
      featureValues = map (getFeatureValue featureIndex) examples
      decisions = map (\fv -> if fv < split then 0 else 1) featureValues
      labels = map processedExampleLabel examples
      possibleDecisions = [0, 1]
      possibleLabels = L.nub labels
    in
      sum $
      map (\(d, l) ->
        let
          pdl = empericalP (zip decisions labels) (d, l)
          pd = empericalP decisions d
          pl = empericalP labels l
        in
          if pdl > 0
            then pdl * log (pdl / (pd * pl))
            else 0) $
      [(d, l) | d <- possibleDecisions, l <- possibleLabels]

featureGoodness :: [ProcessedExample] -> Int -> (Double, Double)
featureGoodness examples featureIndex =
    let
      featureValues =
        foldr (\e l -> if (headMay l) == (Just e) then l else e : l) [] $
        sort $
        map (getFeatureValue featureIndex) examples
      splitPoints =
        map (/2) $
        zipWith (+) featureValues $ drop 1 $ L.cycle featureValues
      goodness = map (\sp -> (sp, splitGoodness examples featureIndex sp)) splitPoints
      bestMay = headMay $ sortWith (\(_, v) -> -v) goodness
    in
      case bestMay of
        Nothing -> (0, 0)
        Just (splitPoint, value) -> (splitPoint, value)

train :: [ProcessedExample] -> DecisionTree
train [] = Decide 0
train (example : examples)
    | all ((== firstExampleLabel) . processedExampleLabel) examples = Decide firstExampleLabel
    | otherwise =
      let
        featureIndexes = [0 .. (length $ processedExampleFeatureValues example) - 1]
        goodness = map (\i -> (i, (featureGoodness (example : examples)) i)) featureIndexes
        bestMay = headMay $ sortWith (\(i, (sp, v)) -> -v) $ goodness
      in
        case bestMay of
          Nothing -> Decide 0
          Just (bestIndex, (sp, v)) ->
            let
              lows = filter (\e -> getFeatureValue bestIndex e < sp) (example : examples)
              highs = filter (\e -> getFeatureValue bestIndex e >= sp) (example : examples)
              lowTree = train lows
              highTree = train highs
            in
              Switch bestIndex sp lowTree highTree
    where firstExampleLabel = processedExampleLabel example

learn :: IO ()
learn = handler learnHandler

data Example = Example
    { exampleImage :: RGB
    , exampleLabel :: Int
    }

data ProcessedExample = ProcessedExample
    { processedExampleFeatureValues :: [Double]
    , processedExampleLabel :: Int
    } deriving (Show)

getFeatureValue :: Int -> ProcessedExample -> Double
getFeatureValue index example =
    indexWithDefault index 0.0 (processedExampleFeatureValues example)

data Feature = Feature RGB

dotWord8 :: Word8 -> Word8 -> Double
dotWord8 a b =
  ((fromIntegral a) / 256) * ((fromIntegral b) / 256)

dotPixel :: RGBPixel -> RGBPixel -> Double
dotPixel p1 p2 =
    (dotWord8 (rgbRed p1) (rgbRed p2)) +
    (dotWord8 (rgbGreen p1) (rgbGreen p2)) +
    (dotWord8 (rgbBlue p1) (rgbBlue p2))

dotImage :: RGB -> RGB -> Double
dotImage i1 i2 =
    V.sum $ V.zipWith dotPixel (I.manifestVector i1) (I.manifestVector i2)

featureValue :: Feature -> RGB -> Double
featureValue (Feature feature) image =
    (dotImage feature image) / (sqrt $ (dotImage feature feature) * (dotImage image image))

loadExample :: [Entity Label] -> Polygon -> Entity Frame -> Handler Example
loadExample labels polygon (Entity frameId frame) = do
    imageEither <- liftIO $ withBinaryFile (unpack $ frameFilename frame) ReadMode (\x -> fmap decodeImage (hGetContents x))
    image <- case imageEither of
      Left error -> invalidArgs ["Could not load image."]
      Right (ImageYCbCr8 image) -> return $ toFridayRGB $ convertImage image
      Right _ -> invalidArgs ["Unknown image type."]
    let chopped = chopImage polygon image

    label <- case headMay $ L.map (\(Entity _ label) -> labelValue label) $ L.filter (\(Entity _ label) -> labelFrame label == frameId) labels of
      Just "unoccupied" -> return 0
      Just "occupied" -> return 1
      other -> invalidArgs ["Bad label " ++ (fromString $ show other) ++ "."]

    return $ Example chopped label

loadExamples :: Int -> Handler [Example]
loadExamples count = do
    let directionId = toSqlKey 3 :: DirectionId
    let regionId = toSqlKey 7 :: RegionId
    frames <- runDB $ selectList [FrameDirection ==. directionId] [LimitTo count]
    labels <- runDB $ selectList [LabelRegion ==. regionId] []
    region <- runDB $ get404 regionId
    polygon <- case (decode (LTE.encodeUtf8 $ LT.fromStrict $ regionValue region) :: Maybe Polygon) of
      Just polygon -> return $ zoom 0.5 polygon
      Nothing -> invalidArgs ["Cannot decode polygon."]
    mapM (loadExample labels polygon) frames

makeFeatures' :: Int -> [Example] -> [Feature]
makeFeatures' 0 _ = []
makeFeatures' _ [] = []
makeFeatures' count (example : examples) =
    (Feature $ exampleImage example) : makeFeatures' (count - 1) (drop ((quot (1 + length examples) count) - 1) examples)

makeFeatures :: Int -> [Example] -> [Feature]
makeFeatures count examples =
    (makeFeatures' (quot count 2) (filter (\x -> exampleLabel x == 0) examples)) ++
    (makeFeatures' (quot count 2) (filter (\x -> exampleLabel x == 1) examples))

processExample :: [Feature] -> Example -> ProcessedExample
processExample features example =
    ProcessedExample
      (map (\f -> featureValue f (exampleImage example)) features)
      (exampleLabel example)

learnHandler :: Handler ()
learnHandler = do
    {-frame <- runDB $ get404 (toSqlKey 4520 :: FrameId)
    region <- runDB $ get404 (toSqlKey 7 :: RegionId)
    let blurred = colorGaussianBlur 5 chopped
    let rejuiced = ImageRGB8 $ toJuicyRGB chopped
    liftIO $ savePngImage "example.png" rejuiced-}
    let trainSize = 200
    let testSize = 500
    let trainTestBufferSize = 100
    examples <- loadExamples (trainSize + trainTestBufferSize + testSize)
    let trainExamples = take trainSize examples
    let testExamples = drop (trainSize + trainTestBufferSize) examples
    let features = makeFeatures 6 trainExamples
    let processedTrainExamples = map (processExample features) trainExamples
    let processedTestExamples = map (processExample features) testExamples
    let tree = train processedTrainExamples
    print tree
    print "Training examples"
    mapM_ (\e ->
      print (processedExampleLabel e, decide tree e)) processedTrainExamples
    print "Testing examples"
    mapM_ (\e ->
      print (processedExampleLabel e, decide tree e)) processedTestExamples


colorGaussianBlur :: Int -> RGB -> RGB
colorGaussianBlur radius image =
    let
      blurF = gaussianBlur radius (Nothing :: Maybe Double)
      red = blurF $ redChannel image
      green = blurF $ greenChannel image
      blue = blurF $ blueChannel image
    in
      combineChannels red green blue


