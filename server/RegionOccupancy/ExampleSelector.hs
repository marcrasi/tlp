module RegionOccupancy.ExampleSelector where

import Application

import Import hiding (fromList, fromString, lookup)

import Codec.Picture (decodeImage)
import Codec.Picture.Types (convertImage, DynamicImage(ImageYCbCr8))
import Data.Time (UTCTime, utctDayTime, getCurrentTime)
import Data.Time.Calendar (Day(ModifiedJulianDay))
import Data.Map.Strict (Map, fromList, lookup)
import Database.Persist.Sql (toSqlKey)
import System.IO (withBinaryFile, IOMode(ReadMode))
import Vision.Image.JuicyPixels (toFridayRGB)

import RegionOccupancy.Example
import RegionOccupancy.OccupancyLabel

data SelectionOptions = SelectionOptions
  { startTime :: UTCTime
  , endTime :: UTCTime
  , exampleCount :: Int
  , occupiedFraction :: Float
  }

defaultSelectionOptions :: SelectionOptions
defaultSelectionOptions = SelectionOptions
  { startTime = UTCTime (ModifiedJulianDay 0) 0
  , endTime = UTCTime (ModifiedJulianDay 0) 0
  , exampleCount = 100
  , occupiedFraction = 0.5
  }

testSelectExamples :: Int64 -> Int -> IO [LabeledExample]
testSelectExamples regionId count = do
    currentTime <- getCurrentTime
    let so = defaultSelectionOptions { endTime = currentTime, exampleCount = count }
    handler $ selectExamples so (toSqlKey regionId)

-- Selects `exampleCount` examples from the database.
-- Gets examples captured between `startTime` and `endTime`.
-- `occupiedFraction` of them are occupied examples.
-- Attempts to dsitribute sample time-of-day as uniformly as possible.
selectExamples :: SelectionOptions -> RegionId -> Handler [LabeledExample]
selectExamples options regionId = do
    region <- runDB $ get404 regionId
    let directionId = regionDirection region
    direction <- runDB $ get404 directionId

    frames <- runDB $ selectList [FrameCapturedAt >=. startTime options, FrameCapturedAt <=. endTime options] []
    labels <- runDB $ selectList [LabelRegion ==. regionId] []
    let labeledFrames = getLabeledFrames frames labels
    let occupiedFrames = map snd $ filter (\t -> (fst t) == Occupied) labeledFrames
    let unoccupiedFrames = map snd $ filter (\t -> (fst t) == Unoccupied) labeledFrames

    let selectedOccupiedFrames = selectFramesUniformly occupiedFrames occupiedExampleCount
    let selectedUnoccupiedFrames = selectFramesUniformly unoccupiedFrames unoccupiedExampleCount

    occupiedExamples <- mapM (loadExample Occupied) selectedOccupiedFrames
    unoccupiedExamples <- mapM (loadExample Unoccupied) selectedUnoccupiedFrames

    return (occupiedExamples ++ unoccupiedExamples)
  where
    occupiedExampleCount = round ((occupiedFraction options) * fromIntegral (exampleCount options))
    unoccupiedExampleCount = (exampleCount options) - occupiedExampleCount

loadExample :: OccupancyLabel -> Entity Frame -> Handler LabeledExample
loadExample label (Entity frameId frame) = do
    imageEither <- liftIO $ withBinaryFile (unpack $ frameFilename frame) ReadMode (\x -> fmap decodeImage (hGetContents x))
    image <- case imageEither of
        Left error -> invalidArgs ["Could not load image."]
        Right (ImageYCbCr8 image) -> return $ toFridayRGB $ convertImage image
        Right _ -> invalidArgs ["Unknown image type."]
    return $ LabeledExample label $ Example (Entity frameId frame) image $ frameCapturedAt frame

captureTimeOfDay :: Entity Frame -> Float
captureTimeOfDay (Entity _ frame) =
  fromIntegral $ floor $ utctDayTime $ frameCapturedAt frame

getLabeledFrames :: [Entity Frame] -> [Entity Label] -> [(OccupancyLabel, Entity Frame)]
getLabeledFrames frames labels =
    concatMap (toList . lookupOccupancyLabel labelMap) frames
  where
    labelMap = fromList $ concatMap (toList . extractOccupancyLabel) labels

lookupOccupancyLabel :: Map FrameId OccupancyLabel -> Entity Frame -> Maybe (OccupancyLabel, Entity Frame)
lookupOccupancyLabel labelMap (Entity frameId frame) =
    fmap (,Entity frameId frame) (lookup frameId labelMap)

extractOccupancyLabel :: Entity Label -> Maybe (FrameId, OccupancyLabel)
extractOccupancyLabel (Entity _ label) =
    fmap (labelFrame label,) (fromText $ labelValue label)

selectFramesUniformly :: [Entity Frame] -> Int -> [Entity Frame]
selectFramesUniformly frames count =
    map fst selectedFrames
  where
    annotatedFrames = map (\f -> (f, captureTimeOfDay f)) frames
    selectedFrames = selectUniformly annotatedFrames count

selectUniformly :: [(a, Float)] -> Int -> [(a, Float)]
selectUniformly list count = selectUniformly' (sortWith snd list) count

selectUniformly' :: [(a, Float)] -> Int -> [(a, Float)]
selectUniformly' [] _ = []
selectUniformly' _ 0 = []
selectUniformly' (xs : x) 1 = [xs]
selectUniformly' (low : remaining) count
    | length sorted <= count = sorted
    | (snd high) == (snd low) = take count sorted
    | otherwise = selectUniformlyFromBuckets buckets count
  where
    sorted = low : remaining
    high = maybe low id $ lastMay remaining
    intervalLength = (snd high) - (snd low)
    bucketSize = intervalLength / fromIntegral count
    bucketLimits = (map (\i -> (snd low) + bucketSize * fromIntegral i) [1 .. count - 1]) ++ [snd high]
    buckets = sortWith (length . snd) (bucket bucketLimits sorted)

selectUniformlyFromBuckets :: [(Float, [(a, Float)])] -> Int -> [(a, Float)]
selectUniformlyFromBuckets [] _ = []
selectUniformlyFromBuckets _ 0 = []
selectUniformlyFromBuckets (bucket : buckets') count =
    (selectUniformly' (snd bucket) willGet) ++ (selectUniformlyFromBuckets buckets' (count - willGet))
  where
    buckets = bucket : buckets'
    needPointsPerBucket = ceiling ((fromIntegral count) / (fromIntegral (length buckets)))
    willGet = min needPointsPerBucket (length $ snd bucket)

-- `bucketLimits` and `points` must be sorted
bucket :: [Float] -> [(a, Float)] -> [(Float, [(a, Float)])]
bucket [] _ = []
bucket bucketLimits [] = map (\p -> (p, [])) bucketLimits
bucket (bucketLimit : bucketLimits) (point : points)
    | snd point <= bucketLimit =
      mapHead
        (\(l, ps) -> (l, point : ps)) $
        bucket (bucketLimit : bucketLimits) points
    | otherwise = (bucketLimit, []) : bucket bucketLimits (point : points)

mapHead :: (a -> a) -> [a] -> [a]
mapHead f [] = []
mapHead f (x : xs) = (f x) : xs
