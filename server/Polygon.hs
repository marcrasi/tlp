module Polygon where

import Import

import Data.List (cycle)

-- A 2d point with coordinates.
data Point = Point Double Double deriving (Show)

px :: Point -> Double
px (Point x _) = x

py :: Point -> Double
py (Point _ y) = y

-- A line segment connecting 2 points, closed at the first point and open
-- at the second point.
data LineSegment = LineSegment Point Point deriving (Show)

-- A rectangle with top left and bottom right corners.
data Rect = Rect Point Point deriving (Show)

-- A polygon with verticies.
data Polygon = Polygon [Point] deriving (Show)

boundingRect :: Polygon -> Rect
boundingRect (Polygon verticies) =
    let
      xMin = maybe 0 id $ minimumMay $ map px verticies
      xMax = maybe 0 id $ maximumMay $ map px verticies
      yMin = maybe 0 id $ minimumMay $ map py verticies
      yMax = maybe 0 id $ maximumMay $ map py verticies
    in
      Rect (Point xMin yMin) (Point xMax yMax)

outsidePoint :: Polygon -> Point
outsidePoint polygon =
    let
      Rect p _ = boundingRect polygon
    in
      Point (px p - 1) (py p - 1)

-- Returns Nothing if the line segments intersect at more than one point!
-- Also returns nonsense if either line segment is a point that lies on the
-- other line segment!
-- Because I am lazy and it's not necessary for the function to work
-- correctly in those cases!
intersectionPoint :: LineSegment -> LineSegment -> Maybe Point
intersectionPoint l1 l2 =
    let
      LineSegment (Point x1 y1) (Point x2 y2) = l1
      LineSegment (Point x3 y3) (Point x4 y4) = l2
      a = x2 - x1
      b = x3 - x4
      c = y2 - y1
      d = y3 - y4
      xo = x3 - x1
      yo = y3 - y1
      determinant = a * d - b * c
    in
      if determinant == 0 then
        Nothing
      else
        let
          t1 = (d * xo - b * yo) / determinant
          t2 = (a * yo - c * xo) / determinant
        in
          if t1 >= 0 && t1 < 1 && t2 >= 0 && t2 < 1 then
            Just $ Point (x1 + t1 * (x2 - x1)) (y1 + t1 * (y2 - y1))
          else
            Nothing

containsPoint :: Polygon -> Point -> Bool 
containsPoint (Polygon verticies) point =
    let
      polygonLines = map (\(p1, p2) -> LineSegment p1 p2) $ zip verticies (drop 1 (cycle verticies))
      lineToOutside = LineSegment point $ outsidePoint (Polygon verticies)
      intersections = map (intersectionPoint lineToOutside) polygonLines 
      intersectionCount = sum $ map (\intersection -> if isJust intersection then 1 else 0) intersections
    in
      intersectionCount `mod` 2 == 1
