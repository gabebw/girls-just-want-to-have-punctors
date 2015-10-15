{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- Common functionality across implementations, like JSON fetching and parsing
-- and an easy way to access the phrase files.

module Common where

import Control.Lens ((.~), (&))
import Network.Wreq
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
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

fileLines :: String -> IO [T.Text]
fileLines n = T.readFile n >>= return . T.splitOn "\n"

phraseFiles :: [String]
phraseFiles = [
    "./phrases/beatles-songs.txt",
    "./phrases/best-selling-books.txt",
    "./phrases/movie-quotes.txt",
    "./phrases/oscar-winning-movies.txt",
    "./phrases/wikipedia-idioms.txt"
    ]
