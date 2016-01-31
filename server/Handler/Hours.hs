{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Hours (getHoursR) where

import Import

import Database.Persist.TH
import Data.Text (Text)
import Database.Persist.Sqlite
import Data.Conduit
import qualified Data.Conduit.List as CL

import Handler.ArgumentParser

data Hour = Hour
  { start :: UTCTime
  , frameCount :: Int
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

getAll :: Handler Value
getAll = do
    let sql = "select datetime((strftime('%s', captured_at) / 3600) * 3600, 'unixepoch') interval, count(1) from frame group by interval order by interval"
    runDB $ rawQuery sql [] $$ CL.mapM_ (liftIO . print)
    returnJson $ (10 :: Int)
