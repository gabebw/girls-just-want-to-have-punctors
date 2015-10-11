{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Control.Lens
import Network.Wreq
import qualified Data.Text as T
import qualified Data.Text.IO as T (readFile)
import Data.Aeson
import GHC.Generics
import Data.Function (on)
import Data.List (maximumBy)
import Control.Monad (liftM)
import Text.Regex.PCRE ((=~))
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as BC

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

-- Filter to phrases that contain any of the rhymes.
-- `phrasesWithWord` will iterate through all of the phrases
-- and `containsAnyOf` will iterate through all of the rhymes.
phrasesWithWord :: [T.Text] -> [T.Text] -> [T.Text]
phrasesWithWord phrases rhymes = filter (containsAnyOf rhymes) phrases

-- Convert Text to ByteString
t2b = BC.pack . T.unpack

-- Does the given phrase contain any of the given rhymes?
containsAnyOf :: [T.Text] -> T.Text -> Bool
containsAnyOf rhymes phrase = t2b phrase =~ anyRhyme
    where
        anyRhyme = t2b $ withWordBoundaries $ T.intercalate "|" rhymes
        withWordBoundaries t = "(?i)\\b(" <> t <> ")\\b"

main = do
    let originalWord = "heart"
    r <- rhymebrainResults originalWord
    let rhymebrainResults = r ^. responseBody
    let highestScoringResults = resultsWithScore (score $ maximum rhymebrainResults) rhymebrainResults
    phrases <- concatMapM fileLines phraseFiles
    let matchingPhrases = phrasesWithWord phrases (map word highestScoringResults)
    print matchingPhrases
