{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- Fetch from the Rhymebrain.com JSON API and parse it into [RhymebrainResult]

module RhymebrainApi where

import Control.Lens ((.~), (&))
import Network.Wreq
import qualified Data.Text as T
import Data.Aeson
import GHC.Generics
import Data.Function (on)
import Control.Monad (liftM)

data RhymebrainResult = RhymebrainResult { score :: Int, word :: T.Text }
    deriving (Generic, FromJSON, Show, Eq)

instance Ord RhymebrainResult where
    compare = compare `on` score

-- Convenience function
concatMapM f x = liftM concat $ mapM f x

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

rhymes :: [RhymebrainResult] -> [T.Text]
rhymes results = map word highestScoringResults
    where
        highestScoringResults = resultsWithScore (score $ maximum results) results
