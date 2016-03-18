module Polygon
    ( Polygon
    , chopImage
    , maskImage
    ) where

import Import

import Data.List (cycle)
import qualified Data.Scientific as Sci
import qualified Data.Vector as Vector
import qualified Vision.Image as I
import qualified Vision.Primitive as P
import qualified Vision.Primitive.Shape as S
import Vision.Image.RGB.Type (RGBPixel)

-- A 2d point with coordinates.
data Point = Point Double Double deriving (Show)

instance FromJSON Point where
    parseJSON (Array a) | length a == 2 =
      Point
        <$> parseJSON (a Vector.! 0)
        <*> parseJSON (a Vector.! 1)
    parseJSON _ = mzero

instance ToJSON Point where
    toJSON (Point x y) =
        Array $ Vector.fromList [Number $ Sci.fromFloatDigits x, Number $ Sci.fromFloatDigits y]

px :: Point -> Double
px (Point x _) = x

py :: Point -> Double
py (Point _ y) = y

-- A line segment connecting 2 points, closed.
data LineSegment = LineSegment Point Point deriving (Show)

-- A rectangle with top left and bottom right corners.
data Rect = Rect Point Point deriving (Show)

rw :: Rect -> Double
rw (Rect (Point x1 _) (Point x2 _)) = x2 - x1

rh :: Rect -> Double
rh (Rect (Point _ y1) (Point _ y2)) = y2 - y1

-- A polygon with verticies.
data Polygon = Polygon [Point] deriving (Show)

instance ToJSON Polygon where
    toJSON (Polygon points) =
        object [ "points" .= points ]

instance FromJSON Polygon where
    parseJSON (Object o) = Polygon
      <$> o .: "points"
    parseJSON _ = mzero

boundingRect :: Polygon -> Rect
boundingRect (Polygon verticies) =
    let
      xMin = maybe 0 id $ minimumMay $ map px verticies
      xMax = maybe 0 id $ maximumMay $ map px verticies
      yMin = maybe 0 id $ minimumMay $ map py verticies
      yMax = maybe 0 id $ maximumMay $ map py verticies
    in
      Rect (Point xMin yMin) (Point xMax yMax)

pairCrossesPlusX :: Point -> Point -> Bool
pairCrossesPlusX a b =
    (((py a) > 0) /= ((py b) > 0)) && (0 < (px b - px a) * (0 - py a) / (py b - py a) + px a)

containsPoint :: Polygon -> Point -> Bool
containsPoint polygon point =
    let
      Polygon vertices = setOrigin point polygon
      vertexPairs = zip vertices (drop 1 (cycle vertices))
      intersectionCount = sum $ map (\(a, b) -> if pairCrossesPlusX a b then 1 else 0) vertexPairs
    in
      intersectionCount `mod` 2 == 1

zoom :: Double -> Polygon -> Polygon
zoom factor (Polygon vertices) =
    Polygon $ map (\(Point x y) -> Point (factor * x) (factor * y)) vertices

setOrigin :: Point -> Polygon -> Polygon
setOrigin (Point ox oy) (Polygon vertices) =
    Polygon $ map (\(Point x y) -> Point (x - ox) (y - oy)) vertices

-- Image processing.
chopImage :: Polygon -> I.RGB -> I.RGB
chopImage unscaledPolygon image =
    let
      rect = boundingRect polygon
      Rect origin _ = rect
      Point ox oy = origin
      translatedPolygon = setOrigin origin polygon
      fridayRect = P.Rect (floor ox) (floor oy) (ceiling $ rw rect) (ceiling $ rh rect)
      croppedImage = I.crop fridayRect image :: I.RGB
    in
      I.fromFunction (I.shape croppedImage) $ \pt ->
        let
          S.Z S.:. y S.:. x = pt
          point = Point (fromIntegral x + 0.5) (fromIntegral y + 0.5)
        in
          if containsPoint translatedPolygon point then
            I.index croppedImage pt
          else
            I.RGBPixel 0 0 0
  where
    -- TODO: Figure out how to get rid of all the duplicated scaling
    -- constants around the code.
    polygon = zoom 0.5 unscaledPolygon

-- Terribly assumes that the image has already been thingied.
maskImage :: Polygon -> I.RGB -> I.DelayedMask I.RGBPixel
maskImage unscaledPolygon croppedImage =
    let
      rect = boundingRect polygon
      Rect origin _ = rect
      Point ox oy = origin
      translatedPolygon = setOrigin origin polygon
      f = (\pt ->
        let
          S.Z S.:. y S.:. x = pt
          point = Point (fromIntegral x + 0.5) (fromIntegral y + 0.5)
        in
          if containsPoint translatedPolygon point then
            Just $ I.index croppedImage pt
          else
            Nothing)
   in
      I.DelayedMask (I.manifestSize croppedImage) f
  where
    -- TODO: Figure out how to get rid of all the duplicated scaling
    -- constants around the code.
    polygon = zoom 0.5 unscaledPolygon
