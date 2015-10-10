{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Network.Wreq
import Data.Text as T

rhymebrainOptions :: T.Text -> Options
rhymebrainOptions word = defaults &
    param "function" .~ ["getRhymes"] &
    param "maxResults" .~ ["0"] &
    param "lang" .~ ["en"] &
    param "word" .~ [word]

rhymebrainHost :: String
rhymebrainHost = "http://rhymebrain.com/talk"

main = do
    let word = "heart"
    r <- getWith (rhymebrainOptions word) rhymebrainHost
    print $ r ^. responseBody
