module RegionOccupancy.ExampleLoader where

import Import

import ExceptHandler
import Image.Loader
import RegionOccupancy.Example

loadExample :: FrameId -> ExceptHandler UnlabeledExample
loadExample frameId = do
    frame <- (lift $ runDB $ get frameId) >>= getOrError "Frame not found."
    image <- loadImage $ unpack $ frameFilename frame
    return $ UnlabeledExample (Entity frameId frame) image
