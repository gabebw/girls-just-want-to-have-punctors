{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Control.Lens
import Network.Wreq
import Data.Text as T (Text)
import Data.Aeson
import GHC.Generics
import Data.Function (on)
import Data.List (maximumBy)

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

rhymebrainResults :: T.Text -> IO (Response [RhymebrainResult])
rhymebrainResults word = asJSON =<< getWith (rhymebrainOptions word) rhymebrainHost

resultsWithScore :: Int -> [RhymebrainResult] -> [RhymebrainResult]
resultsWithScore s = filter (\result -> score result == s)

main = do
    let word = "heart"
    r <- rhymebrainResults word
    let results = r ^. responseBody
    let highestScoring = resultsWithScore (score $ maximum results) results
    print $ highestScoring
