{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Labels where

import Import

getLabelsR :: Handler Value
getLabelsR = returnJson ("hi" :: String) 

postLabelsR :: Handler Value
postLabelsR = returnJson ("hi2" :: String) 
