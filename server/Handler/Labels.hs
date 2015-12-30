{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Labels where

import Import

postLabelsR :: Handler Value
postLabelsR = do
    sendResponseStatus status200 ("OK" :: Text)
