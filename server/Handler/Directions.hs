{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Directions where

import Import hiding (fromList)

import Data.Map (fromList)

import Handler.ResourceResponse

getDirectionsR :: Handler Value
getDirectionsR = do
    directions :: [Entity Direction] <- runDB $ selectList [] []
    intersections :: [Entity Intersection] <- runDB $ selectList [] []
    returnJson $ ResourceResponse
      { elements = directions
      , linked = fromList [("intersections.v1", map toJSON intersections)]
      , pagination = Nothing
      }
