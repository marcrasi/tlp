module ExceptHandler
  ( ExceptHandler
  , getOrError
  , exceptHandler
  , module Control.Monad.Except
  ) where

import Application

import Import
import Control.Monad.Except

type ExceptHandler = ExceptT Text Handler

getOrError :: Text -> Maybe a -> ExceptHandler a
getOrError message (Just value) = return value
getOrError message Nothing = throwError message

exceptHandler :: ExceptHandler a -> IO (Either Text a)
exceptHandler = handler . runExceptT
