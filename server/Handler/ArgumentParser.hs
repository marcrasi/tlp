module Handler.ArgumentParser (getArgument, getOptionalArgument, getPagination, PaginationArgument(start, limit)) where

import Import

import Data.Time.Format

-- TODO: Somehow make this not be an orphan instance.
instance PathPiece UTCTime where
    fromPathPiece = (parseTimeM True defaultTimeLocale "%s") . unpack
    toPathPiece = fromString . (formatTime defaultTimeLocale "%s")

getArgument :: PathPiece a => Text -> Handler a
getArgument argumentName = do
    maybeTextArgument <- lookupGetParam argumentName
    textArgument <- case maybeTextArgument of
      Just text -> return text
      Nothing -> invalidArgs ["Required argument `" ++ argumentName ++ "` not found."]
    parseArgument argumentName textArgument

getOptionalArgument :: PathPiece a => Text -> Handler (Maybe a)
getOptionalArgument argumentName = do
  maybeTextArgument <- lookupGetParam argumentName
  mapM (parseArgument argumentName) maybeTextArgument 

data (PathPiece a) => PaginationArgument a = PaginationArgument
  { start :: Maybe a
  , limit :: Maybe Int
  }

getPagination :: (PathPiece a) => Handler (PaginationArgument a)
getPagination = do
    start <- getOptionalArgument "start"
    limit <- getOptionalArgument "limit"
    return $ PaginationArgument { start = start, limit = limit }

parseArgument :: PathPiece a => Text -> Text -> Handler a
parseArgument argumentName argument =
    case fromPathPiece argument of
      Just fromPathPieced -> return fromPathPieced 
      Nothing -> invalidArgs ["Could not parse '" ++ argument ++ "' for argument `" ++ argumentName ++ "`."]
