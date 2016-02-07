module RegionOccupancy.Feature (Feature(..), evaluate, evaluatePreChopped, preChop) where

import Import

import qualified Data.Vector.Storable as V
import qualified Vision.Image as I

import Polygon

data Feature = Feature
    { featureFrame :: Entity Frame
    , featureImage :: I.RGB
    , featurePolygon :: Polygon
    }

evaluate :: I.RGB -> Feature -> Double
evaluate image feature =
    evaluatePreChopped (preChop image feature) feature

preChop :: I.RGB -> Feature -> I.RGB
preChop image feature =
    chopImage (featurePolygon feature) image

evaluatePreChopped :: I.RGB -> Feature -> Double
evaluatePreChopped cimage feature =
    (dotImage cimage fimage) / (sqrt $ dotImage cimage cimage)
  where
    fimage = featureImage feature

dotWord8 :: Word8 -> Word8 -> Double
dotWord8 a b =
  ((fromIntegral a) / 256) * ((fromIntegral b) / 256)

dotPixel :: I.RGBPixel -> I.RGBPixel -> Double
dotPixel p1 p2 =
    (dotWord8 (I.rgbRed p1) (I.rgbRed p2)) +
    (dotWord8 (I.rgbGreen p1) (I.rgbGreen p2)) +
    (dotWord8 (I.rgbBlue p1) (I.rgbBlue p2))

dotImage :: I.RGB -> I.RGB -> Double
dotImage i1 i2 =
    V.sum $ V.zipWith dotPixel (I.manifestVector i1) (I.manifestVector i2)
