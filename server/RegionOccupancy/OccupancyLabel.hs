module RegionOccupancy.OccupancyLabel where

import Import

data OccupancyLabel
    = Unoccupied
    | Occupied
    deriving (Show, Eq)

fromText :: Text -> Maybe OccupancyLabel
fromText "occupied" = Just Occupied
fromText "unoccupied" = Just Unoccupied
fromText _ = Nothing
