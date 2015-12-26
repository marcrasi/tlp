module Handler.ResourceResponse where

import Import

data Pagination = Pagination
  { next :: Text
  , total :: Maybe Int
  }

instance ToJSON Pagination where
    toJSON (Pagination next total) = object
      [ "next" .= next
      , "total" .= total
      ]

data ResourceResponse e = ResourceResponse
  { elements :: [e]
  , linked :: Map Text [Value]
  , pagination :: Maybe Pagination
  }

instance (ToJSON e) => ToJSON (ResourceResponse e) where
    toJSON (ResourceResponse elements linked pagination) = object
      [ "elements" .= elements
      , "linked" .= linked
      , "pagination" .= pagination
      ]
