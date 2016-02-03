module RegionOccupancy.Knn where

import Import

import RegionOccupancy.Example
import RegionOccupancy.OccupancyLabel

data KnnPoint = KnnPoint
  { label :: OccupancyLabel
  , image :: RGB
  }

data KnnModel = KnnModel
  { regionId :: RegionId
  , points :: [KnnPoint]
  , k :: Int
  }

train :: Int -> [LabeledExample] -> KnnModel
train k labeledExamples = ???
