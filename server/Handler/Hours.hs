{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Hours (getHoursR) where

import Import hiding (fromList)

import Database.Persist.TH
import Data.Text (Text)
import Database.Persist.Sqlite
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Map (fromList)

import Handler.ArgumentParser
import Handler.ResourceResponse

data Hour = Hour
  { start :: Int64
  , frameCount :: Int64
  }

instance ToJSON Hour where
  toJSON (Hour start frameCount) = object
    [ "start" .= start
    , "frameCount" .= frameCount
    ]

getHoursR :: Handler Value
getHoursR = do
    maybeQuery :: Maybe Text <- getOptionalArgument "q"
    case maybeQuery of
      Nothing -> getAll
      Just otherQuery -> invalidArgs ["Unknown query '" ++ otherQuery ++ "'."]

convertFromPersist [PersistInt64 hour, PersistInt64 count] = Just $ Hour hour count
convertFromPersist _ = Nothing

getAll :: Handler Value
getAll = do
    let sql = "select (strftime('%s', captured_at) / 3600) * 3600 interval, count(1) from frame group by interval order by interval"
    hours <- runDB $ rawQuery sql [] $$ CL.map convertFromPersist =$ CL.consume
    returnJson $ ResourceResponse
      { elements = hours
      , linked = fromList []
      , pagination = Nothing
      }
