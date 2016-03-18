module Image.Normalization where

import Import

import qualified Vision.Image as I
import qualified Vision.Histogram as H

equalize :: I.RGB -> I.RGB
equalize = liftToColor $ H.equalizeImage

redChannel :: I.RGB -> I.Grey
redChannel = I.map (I.GreyPixel . I.rgbRed)

greenChannel :: I.RGB -> I.Grey
greenChannel = I.map (I.GreyPixel . I.rgbGreen)

blueChannel :: I.RGB -> I.Grey
blueChannel = I.map (I.GreyPixel . I.rgbBlue)

combineChannels :: I.Grey -> I.Grey -> I.Grey -> I.RGB
combineChannels red green blue =
    I.fromFunction (I.shape red) $ \pt ->
      I.RGBPixel (pixelValue $ I.index red pt) (pixelValue $ I.index green pt) (pixelValue $ I.index blue pt)

pixelValue :: I.GreyPixel -> Word8
pixelValue (I.GreyPixel v) = v

liftToColor :: (I.Grey -> I.Grey) -> I.RGB -> I.RGB
liftToColor f i =
    combineChannels (f $ redChannel i) (f $ greenChannel i) (f $ blueChannel i)

maskToZero :: I.DelayedMask I.GreyPixel -> I.Grey
maskToZero masked = I.fromFunction (I.shape masked) (\pt ->
    case I.maskedIndex masked pt of
        Just pixel -> pixel
        Nothing -> I.GreyPixel 0)

equalizeNonzero :: I.Grey -> I.Grey
equalizeNonzero = maskToZero . H.equalizeImage . zeroToMask
