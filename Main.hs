{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Control.Lens
import Network.Wreq
import Data.Text as T (Text)
import Data.Aeson
import GHC.Generics
import Data.Function (on)

data RhymebrainResult = RhymebrainResult { score :: Int, word :: T.Text  }
    deriving (Generic, FromJSON, Show, Eq)

instance Ord RhymebrainResult where
    compare = compare `on` score

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
    -- print $ r ^. responseBody
    print $ decodeSample
