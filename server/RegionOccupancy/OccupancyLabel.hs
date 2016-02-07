module RegionOccupancy.OccupancyLabel where

import Import

import qualified Data.Map as M

data OccupancyLabel
    = Unoccupied
    | Occupied
    deriving (Show, Eq, Ord)

instance ToJSON OccupancyLabel where
    toJSON occupancyLabel =
        String $ toText occupancyLabel

instance FromJSON OccupancyLabel where
    parseJSON (String str) =
        case fromText str of
            Just result -> return result
            Nothing -> mzero
    parseJSON _ = mzero

fromText :: Text -> Maybe OccupancyLabel
fromText "occupied" = Just Occupied
fromText "unoccupied" = Just Unoccupied
fromText _ = Nothing

toText :: OccupancyLabel -> Text
toText Occupied = "occupied"
toText Unoccupied = "unoccupied"

labelMap :: (Num a) => M.Map OccupancyLabel a
labelMap = M.fromList
    [ (Unoccupied, 0)
    , (Occupied, 0)
    ]
