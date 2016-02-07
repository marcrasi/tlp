module RegionOccupancy.Example where

import Import

import Data.Time (UTCTime)
import Vision.Image.RGB.Type (RGB)

import RegionOccupancy.OccupancyLabel

class Example a where
    getFrame :: a -> Entity Frame
    getImage :: a -> RGB

class Labeled a where
    getLabel :: a -> OccupancyLabel

data UnlabeledExample = UnlabeledExample
  { unlabeledExampleFrame :: Entity Frame
  , unlabeledExampleImage :: RGB
  }

instance Example UnlabeledExample where
    getFrame = unlabeledExampleFrame
    getImage = unlabeledExampleImage

data LabeledExample = LabeledExample
  { labeledExampleLabel :: OccupancyLabel
  , labeledExampleUnlabeledExample :: UnlabeledExample
  }

instance Example LabeledExample where
    getFrame = getFrame . labeledExampleUnlabeledExample
    getImage = getImage . labeledExampleUnlabeledExample

instance Labeled LabeledExample where
    getLabel = labeledExampleLabel
