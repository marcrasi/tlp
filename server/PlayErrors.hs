module PlayErrors where

import Application
import Import

import Control.Monad.Except
import Database.Persist.Sql (toSqlKey)

type FooMonad = ExceptT String Handler

loadThing :: Int64 -> FooMonad Text
loadThing a = do
    labelMay <- lift $ runDB $ get $ toSqlKey a
    label <- case labelMay of
        Just l -> return l
        Nothing -> throwError "Oh my god."
    return $ labelValue label
