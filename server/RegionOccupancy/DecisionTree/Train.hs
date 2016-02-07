module RegionOccupancy.DecisionTree.Train (TrainOptions(..), defaultTrainOptions, train, decide) where

import Import

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector as V

import Polygon
import RegionOccupancy.Example
import RegionOccupancy.Feature
import RegionOccupancy.DecisionTree.Model
import RegionOccupancy.OccupancyLabel

data TrainOptions = TrainOptions
    { featurePerLabelCount :: Int
    , treeDepth :: Int
    }

defaultTrainOptions :: TrainOptions
defaultTrainOptions = TrainOptions
    { featurePerLabelCount = 20
    , treeDepth = 5
    }

class ProcessedExample a where
    getFeatureValues :: a -> V.Vector Double

data UnlabeledProcessedExample = UnlabeledProcessedExample
    { unlabeledProcessedExampleFeatureValues :: V.Vector Double
    }

instance ProcessedExample UnlabeledProcessedExample where
    getFeatureValues = unlabeledProcessedExampleFeatureValues

data LabeledProcessedExample = LabeledProcessedExample
    { labeledProcessedExampleLabel :: OccupancyLabel
    , labeledProcessedExampleUnlabeledProcessedExample :: UnlabeledProcessedExample
    }

instance ProcessedExample LabeledProcessedExample where
    getFeatureValues = getFeatureValues . labeledProcessedExampleUnlabeledProcessedExample

instance Labeled LabeledProcessedExample where
    getLabel = labeledProcessedExampleLabel

type LabeledProcessedExamplePair = (LabeledProcessedExample, LabeledProcessedExample)

instance (ProcessedExample a) => ProcessedExample (a, b) where
    getFeatureValues = getFeatureValues . fst

instance (Labeled a) => Labeled (a, b) where
    getLabel = getLabel . fst

getFeatureValue :: (ProcessedExample a) => Int -> a -> Double
getFeatureValue index example = getFeatureValues example V.! index

decide :: (Example a) => LoadedModel -> a -> OccupancyLabel
decide (LoadedModel _ features tree) example =
  decide' tree $ processExample features example

decide' (Decide label) _ = label
decide' (SwitchDotFrame index threshold lowTree _) example
    | getFeatureValue index example < threshold = decide' lowTree example
decide' (SwitchDotFrame _ _ _ highTree) example = decide' highTree example

processExample :: (Example a) => [Feature] -> a -> UnlabeledProcessedExample
processExample features example =
    UnlabeledProcessedExample $ V.fromList $ map (evaluatePreChopped cimage) features
  where
    -- Optimization that is "wrong" but makes it quite a bit faster.
    cimage = preChop (getImage example) (L.head features)

processLabeledExample :: (Example a, Labeled a) => [Feature] -> a -> LabeledProcessedExample
processLabeledExample features example =
    LabeledProcessedExample (getLabel example) (processExample features example)

train :: (Example a, Labeled a) => TrainOptions -> RegionId -> Polygon -> [a] -> LoadedModel
train (TrainOptions featurePerLabelCount treeDepth) regionId polygon examples =
    pruneModel $ LoadedModel regionId features tree
  where
    featureFrames = selectFeatures featurePerLabelCount examples
    features = map (makeFeature polygon) featureFrames
    processedExamples = map (processLabeledExample features) examples
    tree = train' processedExamples treeDepth

pruneModel :: LoadedModel -> LoadedModel
pruneModel (LoadedModel regionId features tree) =
    LoadedModel regionId prunedFeatures prunedTree
  where
    usedFeatureIndexes = computeOneIntegerPerKey $ sort $ computeUsedFeatureIndexes tree
    prunedFeatures = computePrunedFeatures 0 usedFeatureIndexes features
    prunedTree = mapIndexes (usedFeatureIndexes M.!) tree

mapIndexes :: (Int -> Int) -> DecisionTree -> DecisionTree
mapIndexes f (Decide label) = Decide label
mapIndexes f (SwitchDotFrame index threshold lowTree highTree) =
    SwitchDotFrame (f index) threshold (mapIndexes f lowTree) (mapIndexes f highTree)

computePrunedFeatures :: Int -> M.Map Int Int -> [a] -> [a]
computePrunedFeatures _ _ [] = []
computePrunedFeatures index usedFeatureIndexes (x : xs)
    | member index usedFeatureIndexes = x : remainingPrunedFeatures
    | otherwise = remainingPrunedFeatures
  where
    remainingPrunedFeatures = computePrunedFeatures (index + 1) usedFeatureIndexes xs

computeUsedFeatureIndexes :: DecisionTree -> [Int]
computeUsedFeatureIndexes (Decide _) = []
computeUsedFeatureIndexes (SwitchDotFrame index _ lowTree highTree) =
    index : (computeUsedFeatureIndexes lowTree ++ computeUsedFeatureIndexes highTree)

computeOneIntegerPerKey :: (Ord k) => [k] -> M.Map k Int
computeOneIntegerPerKey keys = computeOneIntegerPerKey' 0 (M.fromList []) keys

computeOneIntegerPerKey' :: (Ord k) => Int -> M.Map k Int -> [k] -> M.Map k Int
computeOneIntegerPerKey' _ startMap [] = startMap
computeOneIntegerPerKey' startInteger startMap (x : xs)
    | member x startMap = computeOneIntegerPerKey' startInteger startMap xs
    | otherwise = computeOneIntegerPerKey' (startInteger + 1) (M.insert x startInteger startMap) xs

train' :: (ProcessedExample a, Labeled a) => [a] -> Int -> DecisionTree
train' [] _ = Decide Unoccupied
train' examples 0 = Decide $ mostCommonLabel examples
train' (example : tailExamples) maxDepth
    | all (\x -> getLabel x == getLabel example) tailExamples = Decide $ getLabel example
    | otherwise =
        case bestFeatureMay of
            Just (index, (threshold, _)) ->
                let
                    lows = filter (\e -> getFeatureValue index e < threshold) examples
                    highs = filter (\e -> getFeatureValue index e >= threshold) examples
                    lowTree = train' lows (maxDepth - 1)
                    highTree = train' highs (maxDepth - 1)
                in
                    SwitchDotFrame index threshold lowTree highTree
            Nothing -> Decide Unoccupied
  where
    examples = example : tailExamples
    featureIndexes = [0 .. (length $ getFeatureValues example) - 1]
    goodnesses = map (\index -> (index, featureGoodness examples index)) featureIndexes
    bestFeatureMay = headMay $ sortWith (\(_, (_, goodness)) -> -goodness) goodnesses

mostCommonLabel :: (Labeled a) => [a] -> OccupancyLabel
mostCommonLabel ls =
    case mostCommonMay of
        Just (mostCommon, _) -> mostCommon
        Nothing -> Unoccupied
  where
    labelCounts = M.assocs $ countLabels ls
    mostCommonMay = headMay $ sortWith (\(_, count) -> -count) labelCounts

featureGoodness :: (ProcessedExample a, Labeled a) => [a] -> Int -> (Double, Double)
featureGoodness examples index =
    case bestSplitMay of
        Just ((ex1, ex2), mi) ->
            (((thisFeatureValue ex1) + (thisFeatureValue ex2)) / 2, mi)
        Nothing -> (0, 0)
  where
    thisFeatureValue = getFeatureValue index
    sortedExamples = sortWith thisFeatureValue examples
    pairedExamples = zip sortedExamples $ drop 1 sortedExamples
    initialOutcomeCounts = M.fromList
        [ (Low, labelMap)
        , (High, countLabels sortedExamples)
        ]
    splitOutcomeCounts = computeSplitOutcomeCounts initialOutcomeCounts pairedExamples
    splitMutualInformations = map
        (\(pair, counts) -> (pair, computeMutualInformation counts))
        splitOutcomeCounts
    bestSplitMay = headMay $ sortWith (\(pair, mi) -> -mi) splitMutualInformations

data Side = Low | High deriving (Show, Eq, Ord)
type LabelCounts = M.Map OccupancyLabel Int
type OutcomeCounts = M.Map Side LabelCounts

labelCountsPlus :: LabelCounts -> LabelCounts -> LabelCounts
labelCountsPlus a b =
    M.unionWith (+) a b

outcomeCountsPlus :: OutcomeCounts -> OutcomeCounts -> OutcomeCounts
outcomeCountsPlus a b =
    M.unionWith labelCountsPlus a b

computeMutualInformation :: OutcomeCounts -> Double
computeMutualInformation counts =
    sum $ M.elems $ M.mapWithKey (\side labelCounts ->
        sum $ M.elems $ M.mapWithKey (\label outcomeCount ->
            let
                nxy = fromIntegral $ outcomeCount
                pxy = nxy / nSamples
                nx = fromIntegral $ nSide M.! side
                px = nx / nSamples
                ny = fromIntegral $ nLabel M.! label
                py = ny / nSamples
            in
                if nxy > 0
                    then pxy * log (pxy / (px * py))
                    else 0) labelCounts) counts
  where
    nSide = M.map (sum . M.elems) counts
    nLabel = M.unionsWith (+) $ M.elems counts
    nSamples = fromIntegral $ sum $ M.elems nSide

computeSplitOutcomeCounts :: (Labeled a) => OutcomeCounts -> [a] -> [(a, OutcomeCounts)]
computeSplitOutcomeCounts start [] = []
computeSplitOutcomeCounts start (l : ls) =
    (l, withThis) : computeSplitOutcomeCounts withThis ls
  where
    outcomeDelta = M.fromList
        [ (Low, M.fromList [(getLabel l, 1)])
        , (High, M.fromList [(getLabel l, -1)])
        ]
    withThis = outcomeCountsPlus outcomeDelta start

countLabels :: (Labeled a) => [a] -> LabelCounts
countLabels [] = labelMap
countLabels (l : ls) =
    labelCountsPlus (M.fromList [(getLabel l, 1)]) (countLabels ls)

allTheSame :: (Eq a) => [a] -> Bool
allTheSame [] = True
allTheSame (x : xs) = all (== x) xs

makeFeature :: (Example a) => Polygon -> a -> Feature
makeFeature polygon example =
    Feature (getFrame example) (chopImage polygon $ getImage example) polygon

selectFeatures :: (Labeled a) => Int -> [a] -> [a]
selectFeatures featurePerLabelCount examples =
    concatMap (selectFeatures' featurePerLabelCount examples) [Occupied, Unoccupied]

selectFeatures' :: (Labeled a) => Int -> [a] -> OccupancyLabel -> [a]
selectFeatures' takeCount examples label =
    uniformTake takeCount $ filter (\e -> getLabel e == label) examples

uniformTake :: Int -> [a] -> [a]
uniformTake 0 _ = []
uniformTake _ [] = []
uniformTake takeCount (x : xs) =
    x : (uniformTake (takeCount - 1) $ drop dropCount xs)
  where
    dropCount = ((length xs + 1) `quot` takeCount) - 1
