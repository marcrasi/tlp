module ExceptHandler
  ( ExceptHandler
  , getOrError
  , errorToInternalServer
  , module Control.Monad.Except
  ) where

import Import
import Control.Monad.Except (ExceptT, throwError, runExceptT)

type ExceptHandler = ExceptT Text Handler

getOrError :: Text -> Maybe a -> ExceptHandler a
getOrError message (Just value) = return value
getOrError message Nothing = throwError message

errorToInternalServer :: ExceptHandler a -> Handler a
errorToInternalServer exceptHandler = do
    result <- runExceptT exceptHandler
    case result of
        Left error -> invalidArgs [error]
        Right success -> return success
